{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- Module      : Test.AWS.Data.Map
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data.Map (tests) where

import GHC.Exts
import Network.AWS.Prelude
import Test.AWS.TH
import Test.AWS.Types
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "map"
    [ testGroup "deserialise xml"
        [ testCase "entries" $
            assertXML unflattened (Entries entries)
        , testCase "flattened" $
            assertXML flattened entries
        ]
    ]

entries :: EMap "entry" "key" "value" Text Text
entries = fromList
    [ ("foo", "828ef3fdfa96f00ad9f27c383fc9ac7")
    , ("bar", "fe3f123jfa96f00ad9f27c383fc9sd1")
    , ("baz", "e50e9ce187d0a3a62f2ac0a5d89bf0a")
    ]

unflattened :: LazyByteString
unflattened = [doc|
    <?xml version="1.0" encoding="UTF-8"?>
    <Wrapper>
      <Random>N</Random>
      <Entries>
        <entry>
          <key>baz</key>
          <value>e50e9ce187d0a3a62f2ac0a5d89bf0a</value>
        </entry>
        <entry>
          <key>foo</key>
          <value>828ef3fdfa96f00ad9f27c383fc9ac7</value>
        </entry>
        <entry>
          <key>bar</key>
          <value>fe3f123jfa96f00ad9f27c383fc9sd1</value>
        </entry>
      </Entries>
      <Item>test</Item>
    </Wrapper>
    |]

flattened :: LazyByteString
flattened = [doc|
    <?xml version="1.0" encoding="UTF-8"?>
    <Wrapper>
      <Foo>foo</Foo>
      <Bar>N</Bar>
      <entry>
        <key>foo</key>
        <value>828ef3fdfa96f00ad9f27c383fc9ac7</value>
      </entry>
      <entry>
        <key>bar</key>
        <value>fe3f123jfa96f00ad9f27c383fc9sd1</value>
      </entry>
      <entry>
        <key>baz</key>
        <value>e50e9ce187d0a3a62f2ac0a5d89bf0a</value>
      </entry>
      <Baz>Ned</Baz>
    </Wrapper>
    |]

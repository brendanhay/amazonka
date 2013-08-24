{-# LANGUAGE TemplateHaskell      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Test.Signing
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.Signing (tests) where

import Network.AWS.Internal
import Test.Common

tests :: [Test]
tests =
    [ testVersion "Version4"
        [

        ]
    ]

-- <file-name>.req—the web request to be signed.
-- <file-name>.creq—the resulting canonical request.
-- <file-name>.sts—the resulting string to sign.
-- <file-name>.authz—the authorization header.
-- <file-name>.sreq— the signed request.

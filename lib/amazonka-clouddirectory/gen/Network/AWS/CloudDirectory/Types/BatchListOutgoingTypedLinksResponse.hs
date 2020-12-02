{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListOutgoingTypedLinksResponse where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListOutgoingTypedLinks' response operation.
--
--
--
-- /See:/ 'batchListOutgoingTypedLinksResponse' smart constructor.
data BatchListOutgoingTypedLinksResponse = BatchListOutgoingTypedLinksResponse'
  { _blotlTypedLinkSpecifiers ::
      !( Maybe
           [TypedLinkSpecifier]
       ),
    _blotlNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListOutgoingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blotlTypedLinkSpecifiers' - Returns a typed link specifier as output.
--
-- * 'blotlNextToken' - The pagination token.
batchListOutgoingTypedLinksResponse ::
  BatchListOutgoingTypedLinksResponse
batchListOutgoingTypedLinksResponse =
  BatchListOutgoingTypedLinksResponse'
    { _blotlTypedLinkSpecifiers =
        Nothing,
      _blotlNextToken = Nothing
    }

-- | Returns a typed link specifier as output.
blotlTypedLinkSpecifiers :: Lens' BatchListOutgoingTypedLinksResponse [TypedLinkSpecifier]
blotlTypedLinkSpecifiers = lens _blotlTypedLinkSpecifiers (\s a -> s {_blotlTypedLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
blotlNextToken :: Lens' BatchListOutgoingTypedLinksResponse (Maybe Text)
blotlNextToken = lens _blotlNextToken (\s a -> s {_blotlNextToken = a})

instance FromJSON BatchListOutgoingTypedLinksResponse where
  parseJSON =
    withObject
      "BatchListOutgoingTypedLinksResponse"
      ( \x ->
          BatchListOutgoingTypedLinksResponse'
            <$> (x .:? "TypedLinkSpecifiers" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListOutgoingTypedLinksResponse

instance NFData BatchListOutgoingTypedLinksResponse

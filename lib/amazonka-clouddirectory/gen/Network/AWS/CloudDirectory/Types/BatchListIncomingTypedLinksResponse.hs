{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchListIncomingTypedLinksResponse where

import Network.AWS.CloudDirectory.Types.TypedLinkSpecifier
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of a 'ListIncomingTypedLinks' response operation.
--
--
--
-- /See:/ 'batchListIncomingTypedLinksResponse' smart constructor.
data BatchListIncomingTypedLinksResponse = BatchListIncomingTypedLinksResponse'
  { _blitlLinkSpecifiers ::
      !( Maybe
           [TypedLinkSpecifier]
       ),
    _blitlNextToken ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchListIncomingTypedLinksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'blitlLinkSpecifiers' - Returns one or more typed link specifiers as output.
--
-- * 'blitlNextToken' - The pagination token.
batchListIncomingTypedLinksResponse ::
  BatchListIncomingTypedLinksResponse
batchListIncomingTypedLinksResponse =
  BatchListIncomingTypedLinksResponse'
    { _blitlLinkSpecifiers =
        Nothing,
      _blitlNextToken = Nothing
    }

-- | Returns one or more typed link specifiers as output.
blitlLinkSpecifiers :: Lens' BatchListIncomingTypedLinksResponse [TypedLinkSpecifier]
blitlLinkSpecifiers = lens _blitlLinkSpecifiers (\s a -> s {_blitlLinkSpecifiers = a}) . _Default . _Coerce

-- | The pagination token.
blitlNextToken :: Lens' BatchListIncomingTypedLinksResponse (Maybe Text)
blitlNextToken = lens _blitlNextToken (\s a -> s {_blitlNextToken = a})

instance FromJSON BatchListIncomingTypedLinksResponse where
  parseJSON =
    withObject
      "BatchListIncomingTypedLinksResponse"
      ( \x ->
          BatchListIncomingTypedLinksResponse'
            <$> (x .:? "LinkSpecifiers" .!= mempty) <*> (x .:? "NextToken")
      )

instance Hashable BatchListIncomingTypedLinksResponse

instance NFData BatchListIncomingTypedLinksResponse

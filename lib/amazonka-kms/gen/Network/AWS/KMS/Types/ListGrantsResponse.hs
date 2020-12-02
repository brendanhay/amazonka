{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ListGrantsResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ListGrantsResponse where

import Network.AWS.KMS.Types.GrantListEntry
import Network.AWS.Lens
import Network.AWS.Prelude

-- | /See:/ 'listGrantsResponse' smart constructor.
data ListGrantsResponse = ListGrantsResponse'
  { _lgTruncated ::
      !(Maybe Bool),
    _lgGrants :: !(Maybe [GrantListEntry]),
    _lgNextMarker :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGrantsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgTruncated' - A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
--
-- * 'lgGrants' - A list of grants.
--
-- * 'lgNextMarker' - When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
listGrantsResponse ::
  ListGrantsResponse
listGrantsResponse =
  ListGrantsResponse'
    { _lgTruncated = Nothing,
      _lgGrants = Nothing,
      _lgNextMarker = Nothing
    }

-- | A flag that indicates whether there are more items in the list. When this value is true, the list in this response is truncated. To get more items, pass the value of the @NextMarker@ element in thisresponse to the @Marker@ parameter in a subsequent request.
lgTruncated :: Lens' ListGrantsResponse (Maybe Bool)
lgTruncated = lens _lgTruncated (\s a -> s {_lgTruncated = a})

-- | A list of grants.
lgGrants :: Lens' ListGrantsResponse [GrantListEntry]
lgGrants = lens _lgGrants (\s a -> s {_lgGrants = a}) . _Default . _Coerce

-- | When @Truncated@ is true, this element is present and contains the value to use for the @Marker@ parameter in a subsequent request.
lgNextMarker :: Lens' ListGrantsResponse (Maybe Text)
lgNextMarker = lens _lgNextMarker (\s a -> s {_lgNextMarker = a})

instance FromJSON ListGrantsResponse where
  parseJSON =
    withObject
      "ListGrantsResponse"
      ( \x ->
          ListGrantsResponse'
            <$> (x .:? "Truncated")
            <*> (x .:? "Grants" .!= mempty)
            <*> (x .:? "NextMarker")
      )

instance Hashable ListGrantsResponse

instance NFData ListGrantsResponse

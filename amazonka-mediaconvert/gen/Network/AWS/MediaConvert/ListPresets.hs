{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.ListPresets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your presets. This will return the presets themselves, not just a list of them. To retrieve the next twenty presets, use the nextToken string returned with the array.
module Network.AWS.MediaConvert.ListPresets
    (
    -- * Creating a Request
      listPresets
    , ListPresets
    -- * Request Lenses
    , lpCategory
    , lpListBy
    , lpNextToken
    , lpOrder
    , lpMaxResults

    -- * Destructuring the Response
    , listPresetsResponse
    , ListPresetsResponse
    -- * Response Lenses
    , lprsPresets
    , lprsNextToken
    , lprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MediaConvert.Types
import Network.AWS.MediaConvert.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listPresets' smart constructor.
data ListPresets = ListPresets'
  { _lpCategory   :: !(Maybe Text)
  , _lpListBy     :: !(Maybe PresetListBy)
  , _lpNextToken  :: !(Maybe Text)
  , _lpOrder      :: !(Maybe Order)
  , _lpMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPresets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lpCategory' - Optionally, specify a preset category to limit responses to only presets from that category.
--
-- * 'lpListBy' - Undocumented member.
--
-- * 'lpNextToken' - Use this string, provided with the response to a previous request, to request the next batch of presets.
--
-- * 'lpOrder' - Undocumented member.
--
-- * 'lpMaxResults' - Optional. Number of presets, up to twenty, that will be returned at one time
listPresets
    :: ListPresets
listPresets =
  ListPresets'
    { _lpCategory = Nothing
    , _lpListBy = Nothing
    , _lpNextToken = Nothing
    , _lpOrder = Nothing
    , _lpMaxResults = Nothing
    }


-- | Optionally, specify a preset category to limit responses to only presets from that category.
lpCategory :: Lens' ListPresets (Maybe Text)
lpCategory = lens _lpCategory (\ s a -> s{_lpCategory = a})

-- | Undocumented member.
lpListBy :: Lens' ListPresets (Maybe PresetListBy)
lpListBy = lens _lpListBy (\ s a -> s{_lpListBy = a})

-- | Use this string, provided with the response to a previous request, to request the next batch of presets.
lpNextToken :: Lens' ListPresets (Maybe Text)
lpNextToken = lens _lpNextToken (\ s a -> s{_lpNextToken = a})

-- | Undocumented member.
lpOrder :: Lens' ListPresets (Maybe Order)
lpOrder = lens _lpOrder (\ s a -> s{_lpOrder = a})

-- | Optional. Number of presets, up to twenty, that will be returned at one time
lpMaxResults :: Lens' ListPresets (Maybe Int)
lpMaxResults = lens _lpMaxResults (\ s a -> s{_lpMaxResults = a})

instance AWSRequest ListPresets where
        type Rs ListPresets = ListPresetsResponse
        request = get mediaConvert
        response
          = receiveJSON
              (\ s h x ->
                 ListPresetsResponse' <$>
                   (x .?> "presets" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListPresets where

instance NFData ListPresets where

instance ToHeaders ListPresets where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath ListPresets where
        toPath = const "/2017-08-29/presets"

instance ToQuery ListPresets where
        toQuery ListPresets'{..}
          = mconcat
              ["category" =: _lpCategory, "listBy" =: _lpListBy,
               "nextToken" =: _lpNextToken, "order" =: _lpOrder,
               "maxResults" =: _lpMaxResults]

-- | /See:/ 'listPresetsResponse' smart constructor.
data ListPresetsResponse = ListPresetsResponse'
  { _lprsPresets        :: !(Maybe [Preset])
  , _lprsNextToken      :: !(Maybe Text)
  , _lprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListPresetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lprsPresets' - List of presets
--
-- * 'lprsNextToken' - Use this string to request the next batch of presets.
--
-- * 'lprsResponseStatus' - -- | The response status code.
listPresetsResponse
    :: Int -- ^ 'lprsResponseStatus'
    -> ListPresetsResponse
listPresetsResponse pResponseStatus_ =
  ListPresetsResponse'
    { _lprsPresets = Nothing
    , _lprsNextToken = Nothing
    , _lprsResponseStatus = pResponseStatus_
    }


-- | List of presets
lprsPresets :: Lens' ListPresetsResponse [Preset]
lprsPresets = lens _lprsPresets (\ s a -> s{_lprsPresets = a}) . _Default . _Coerce

-- | Use this string to request the next batch of presets.
lprsNextToken :: Lens' ListPresetsResponse (Maybe Text)
lprsNextToken = lens _lprsNextToken (\ s a -> s{_lprsNextToken = a})

-- | -- | The response status code.
lprsResponseStatus :: Lens' ListPresetsResponse Int
lprsResponseStatus = lens _lprsResponseStatus (\ s a -> s{_lprsResponseStatus = a})

instance NFData ListPresetsResponse where

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
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all field-level encryption configurations that have been created in CloudFront for this account.
--
--
module Network.AWS.CloudFront.ListFieldLevelEncryptionConfigs
    (
    -- * Creating a Request
      listFieldLevelEncryptionConfigs
    , ListFieldLevelEncryptionConfigs
    -- * Request Lenses
    , lflecMarker
    , lflecMaxItems

    -- * Destructuring the Response
    , listFieldLevelEncryptionConfigsResponse
    , ListFieldLevelEncryptionConfigsResponse
    -- * Response Lenses
    , lflecrsFieldLevelEncryptionList
    , lflecrsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFieldLevelEncryptionConfigs' smart constructor.
data ListFieldLevelEncryptionConfigs = ListFieldLevelEncryptionConfigs'
  { _lflecMarker   :: !(Maybe Text)
  , _lflecMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFieldLevelEncryptionConfigs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lflecMarker' - Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
--
-- * 'lflecMaxItems' - The maximum number of field-level encryption configurations you want in the response body.
listFieldLevelEncryptionConfigs
    :: ListFieldLevelEncryptionConfigs
listFieldLevelEncryptionConfigs =
  ListFieldLevelEncryptionConfigs'
    {_lflecMarker = Nothing, _lflecMaxItems = Nothing}


-- | Use this when paginating results to indicate where to begin in your list of configurations. The results include configurations in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last configuration on that page).
lflecMarker :: Lens' ListFieldLevelEncryptionConfigs (Maybe Text)
lflecMarker = lens _lflecMarker (\ s a -> s{_lflecMarker = a})

-- | The maximum number of field-level encryption configurations you want in the response body.
lflecMaxItems :: Lens' ListFieldLevelEncryptionConfigs (Maybe Text)
lflecMaxItems = lens _lflecMaxItems (\ s a -> s{_lflecMaxItems = a})

instance AWSRequest ListFieldLevelEncryptionConfigs
         where
        type Rs ListFieldLevelEncryptionConfigs =
             ListFieldLevelEncryptionConfigsResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListFieldLevelEncryptionConfigsResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable ListFieldLevelEncryptionConfigs
         where

instance NFData ListFieldLevelEncryptionConfigs where

instance ToHeaders ListFieldLevelEncryptionConfigs
         where
        toHeaders = const mempty

instance ToPath ListFieldLevelEncryptionConfigs where
        toPath = const "/2017-10-30/field-level-encryption"

instance ToQuery ListFieldLevelEncryptionConfigs
         where
        toQuery ListFieldLevelEncryptionConfigs'{..}
          = mconcat
              ["Marker" =: _lflecMarker,
               "MaxItems" =: _lflecMaxItems]

-- | /See:/ 'listFieldLevelEncryptionConfigsResponse' smart constructor.
data ListFieldLevelEncryptionConfigsResponse = ListFieldLevelEncryptionConfigsResponse'
  { _lflecrsFieldLevelEncryptionList :: !(Maybe FieldLevelEncryptionList)
  , _lflecrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFieldLevelEncryptionConfigsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lflecrsFieldLevelEncryptionList' - Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
--
-- * 'lflecrsResponseStatus' - -- | The response status code.
listFieldLevelEncryptionConfigsResponse
    :: Int -- ^ 'lflecrsResponseStatus'
    -> ListFieldLevelEncryptionConfigsResponse
listFieldLevelEncryptionConfigsResponse pResponseStatus_ =
  ListFieldLevelEncryptionConfigsResponse'
    { _lflecrsFieldLevelEncryptionList = Nothing
    , _lflecrsResponseStatus = pResponseStatus_
    }


-- | Returns a list of all field-level encryption configurations that have been created in CloudFront for this account.
lflecrsFieldLevelEncryptionList :: Lens' ListFieldLevelEncryptionConfigsResponse (Maybe FieldLevelEncryptionList)
lflecrsFieldLevelEncryptionList = lens _lflecrsFieldLevelEncryptionList (\ s a -> s{_lflecrsFieldLevelEncryptionList = a})

-- | -- | The response status code.
lflecrsResponseStatus :: Lens' ListFieldLevelEncryptionConfigsResponse Int
lflecrsResponseStatus = lens _lflecrsResponseStatus (\ s a -> s{_lflecrsResponseStatus = a})

instance NFData
           ListFieldLevelEncryptionConfigsResponse
         where

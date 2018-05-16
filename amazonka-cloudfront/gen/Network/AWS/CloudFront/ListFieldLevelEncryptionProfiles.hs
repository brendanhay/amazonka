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
-- Module      : Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Request a list of field-level encryption profiles that have been created in CloudFront for this account.
--
--
module Network.AWS.CloudFront.ListFieldLevelEncryptionProfiles
    (
    -- * Creating a Request
      listFieldLevelEncryptionProfiles
    , ListFieldLevelEncryptionProfiles
    -- * Request Lenses
    , lflepMarker
    , lflepMaxItems

    -- * Destructuring the Response
    , listFieldLevelEncryptionProfilesResponse
    , ListFieldLevelEncryptionProfilesResponse
    -- * Response Lenses
    , lfleprsFieldLevelEncryptionProfileList
    , lfleprsResponseStatus
    ) where

import Network.AWS.CloudFront.Types
import Network.AWS.CloudFront.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listFieldLevelEncryptionProfiles' smart constructor.
data ListFieldLevelEncryptionProfiles = ListFieldLevelEncryptionProfiles'
  { _lflepMarker   :: !(Maybe Text)
  , _lflepMaxItems :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFieldLevelEncryptionProfiles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lflepMarker' - Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
--
-- * 'lflepMaxItems' - The maximum number of field-level encryption profiles you want in the response body.
listFieldLevelEncryptionProfiles
    :: ListFieldLevelEncryptionProfiles
listFieldLevelEncryptionProfiles =
  ListFieldLevelEncryptionProfiles'
    {_lflepMarker = Nothing, _lflepMaxItems = Nothing}


-- | Use this when paginating results to indicate where to begin in your list of profiles. The results include profiles in the list that occur after the marker. To get the next page of results, set the @Marker@ to the value of the @NextMarker@ from the current page's response (which is also the ID of the last profile on that page).
lflepMarker :: Lens' ListFieldLevelEncryptionProfiles (Maybe Text)
lflepMarker = lens _lflepMarker (\ s a -> s{_lflepMarker = a})

-- | The maximum number of field-level encryption profiles you want in the response body.
lflepMaxItems :: Lens' ListFieldLevelEncryptionProfiles (Maybe Text)
lflepMaxItems = lens _lflepMaxItems (\ s a -> s{_lflepMaxItems = a})

instance AWSRequest ListFieldLevelEncryptionProfiles
         where
        type Rs ListFieldLevelEncryptionProfiles =
             ListFieldLevelEncryptionProfilesResponse
        request = get cloudFront
        response
          = receiveXML
              (\ s h x ->
                 ListFieldLevelEncryptionProfilesResponse' <$>
                   (parseXML x) <*> (pure (fromEnum s)))

instance Hashable ListFieldLevelEncryptionProfiles
         where

instance NFData ListFieldLevelEncryptionProfiles
         where

instance ToHeaders ListFieldLevelEncryptionProfiles
         where
        toHeaders = const mempty

instance ToPath ListFieldLevelEncryptionProfiles
         where
        toPath
          = const "/2017-10-30/field-level-encryption-profile"

instance ToQuery ListFieldLevelEncryptionProfiles
         where
        toQuery ListFieldLevelEncryptionProfiles'{..}
          = mconcat
              ["Marker" =: _lflepMarker,
               "MaxItems" =: _lflepMaxItems]

-- | /See:/ 'listFieldLevelEncryptionProfilesResponse' smart constructor.
data ListFieldLevelEncryptionProfilesResponse = ListFieldLevelEncryptionProfilesResponse'
  { _lfleprsFieldLevelEncryptionProfileList :: !(Maybe FieldLevelEncryptionProfileList)
  , _lfleprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListFieldLevelEncryptionProfilesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lfleprsFieldLevelEncryptionProfileList' - Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
--
-- * 'lfleprsResponseStatus' - -- | The response status code.
listFieldLevelEncryptionProfilesResponse
    :: Int -- ^ 'lfleprsResponseStatus'
    -> ListFieldLevelEncryptionProfilesResponse
listFieldLevelEncryptionProfilesResponse pResponseStatus_ =
  ListFieldLevelEncryptionProfilesResponse'
    { _lfleprsFieldLevelEncryptionProfileList = Nothing
    , _lfleprsResponseStatus = pResponseStatus_
    }


-- | Returns a list of the field-level encryption profiles that have been created in CloudFront for this account.
lfleprsFieldLevelEncryptionProfileList :: Lens' ListFieldLevelEncryptionProfilesResponse (Maybe FieldLevelEncryptionProfileList)
lfleprsFieldLevelEncryptionProfileList = lens _lfleprsFieldLevelEncryptionProfileList (\ s a -> s{_lfleprsFieldLevelEncryptionProfileList = a})

-- | -- | The response status code.
lfleprsResponseStatus :: Lens' ListFieldLevelEncryptionProfilesResponse Int
lfleprsResponseStatus = lens _lfleprsResponseStatus (\ s a -> s{_lfleprsResponseStatus = a})

instance NFData
           ListFieldLevelEncryptionProfilesResponse
         where

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
-- Module      : Network.AWS.Greengrass.GetGroupVersion
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group version.
module Network.AWS.Greengrass.GetGroupVersion
    (
    -- * Creating a Request
      getGroupVersion
    , GetGroupVersion
    -- * Request Lenses
    , ggvGroupVersionId
    , ggvGroupId

    -- * Destructuring the Response
    , getGroupVersionResponse
    , GetGroupVersionResponse
    -- * Response Lenses
    , ggvrsDefinition
    , ggvrsARN
    , ggvrsCreationTimestamp
    , ggvrsVersion
    , ggvrsId
    , ggvrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroupVersion' smart constructor.
data GetGroupVersion = GetGroupVersion'
    { _ggvGroupVersionId :: !Text
    , _ggvGroupId        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggvGroupVersionId' - Group version Id
--
-- * 'ggvGroupId' - The unique Id of the AWS Greengrass Group
getGroupVersion
    :: Text -- ^ 'ggvGroupVersionId'
    -> Text -- ^ 'ggvGroupId'
    -> GetGroupVersion
getGroupVersion pGroupVersionId_ pGroupId_ =
    GetGroupVersion'
    { _ggvGroupVersionId = pGroupVersionId_
    , _ggvGroupId = pGroupId_
    }

-- | Group version Id
ggvGroupVersionId :: Lens' GetGroupVersion Text
ggvGroupVersionId = lens _ggvGroupVersionId (\ s a -> s{_ggvGroupVersionId = a});

-- | The unique Id of the AWS Greengrass Group
ggvGroupId :: Lens' GetGroupVersion Text
ggvGroupId = lens _ggvGroupId (\ s a -> s{_ggvGroupId = a});

instance AWSRequest GetGroupVersion where
        type Rs GetGroupVersion = GetGroupVersionResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupVersionResponse' <$>
                   (x .?> "Definition") <*> (x .?> "Arn") <*>
                     (x .?> "CreationTimestamp")
                     <*> (x .?> "Version")
                     <*> (x .?> "Id")
                     <*> (pure (fromEnum s)))

instance Hashable GetGroupVersion

instance NFData GetGroupVersion

instance ToHeaders GetGroupVersion where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGroupVersion where
        toPath GetGroupVersion'{..}
          = mconcat
              ["/greengrass/groups/", toBS _ggvGroupId,
               "/versions/", toBS _ggvGroupVersionId]

instance ToQuery GetGroupVersion where
        toQuery = const mempty

-- | /See:/ 'getGroupVersionResponse' smart constructor.
data GetGroupVersionResponse = GetGroupVersionResponse'
    { _ggvrsDefinition        :: !(Maybe GroupVersion)
    , _ggvrsARN               :: !(Maybe Text)
    , _ggvrsCreationTimestamp :: !(Maybe Text)
    , _ggvrsVersion           :: !(Maybe Text)
    , _ggvrsId                :: !(Maybe Text)
    , _ggvrsResponseStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggvrsDefinition' - Information on the definition
--
-- * 'ggvrsARN' - Arn of the group version.
--
-- * 'ggvrsCreationTimestamp' - Timestamp when the group version was created.
--
-- * 'ggvrsVersion' - Unique Id for a version of the Group.
--
-- * 'ggvrsId' - Id of the group version.
--
-- * 'ggvrsResponseStatus' - -- | The response status code.
getGroupVersionResponse
    :: Int -- ^ 'ggvrsResponseStatus'
    -> GetGroupVersionResponse
getGroupVersionResponse pResponseStatus_ =
    GetGroupVersionResponse'
    { _ggvrsDefinition = Nothing
    , _ggvrsARN = Nothing
    , _ggvrsCreationTimestamp = Nothing
    , _ggvrsVersion = Nothing
    , _ggvrsId = Nothing
    , _ggvrsResponseStatus = pResponseStatus_
    }

-- | Information on the definition
ggvrsDefinition :: Lens' GetGroupVersionResponse (Maybe GroupVersion)
ggvrsDefinition = lens _ggvrsDefinition (\ s a -> s{_ggvrsDefinition = a});

-- | Arn of the group version.
ggvrsARN :: Lens' GetGroupVersionResponse (Maybe Text)
ggvrsARN = lens _ggvrsARN (\ s a -> s{_ggvrsARN = a});

-- | Timestamp when the group version was created.
ggvrsCreationTimestamp :: Lens' GetGroupVersionResponse (Maybe Text)
ggvrsCreationTimestamp = lens _ggvrsCreationTimestamp (\ s a -> s{_ggvrsCreationTimestamp = a});

-- | Unique Id for a version of the Group.
ggvrsVersion :: Lens' GetGroupVersionResponse (Maybe Text)
ggvrsVersion = lens _ggvrsVersion (\ s a -> s{_ggvrsVersion = a});

-- | Id of the group version.
ggvrsId :: Lens' GetGroupVersionResponse (Maybe Text)
ggvrsId = lens _ggvrsId (\ s a -> s{_ggvrsId = a});

-- | -- | The response status code.
ggvrsResponseStatus :: Lens' GetGroupVersionResponse Int
ggvrsResponseStatus = lens _ggvrsResponseStatus (\ s a -> s{_ggvrsResponseStatus = a});

instance NFData GetGroupVersionResponse

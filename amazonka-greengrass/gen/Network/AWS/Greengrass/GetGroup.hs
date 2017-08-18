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
-- Module      : Network.AWS.Greengrass.GetGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group.
module Network.AWS.Greengrass.GetGroup
    (
    -- * Creating a Request
      getGroup
    , GetGroup
    -- * Request Lenses
    , ggGroupId

    -- * Destructuring the Response
    , getGroupResponse
    , GetGroupResponse
    -- * Response Lenses
    , ggrsLatestVersionARN
    , ggrsARN
    , ggrsName
    , ggrsCreationTimestamp
    , ggrsId
    , ggrsLatestVersion
    , ggrsLastUpdatedTimestamp
    , ggrsResponseStatus
    ) where

import           Network.AWS.Greengrass.Types
import           Network.AWS.Greengrass.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getGroup' smart constructor.
newtype GetGroup = GetGroup'
    { _ggGroupId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggGroupId' - The unique Id of the AWS Greengrass Group
getGroup
    :: Text -- ^ 'ggGroupId'
    -> GetGroup
getGroup pGroupId_ =
    GetGroup'
    { _ggGroupId = pGroupId_
    }

-- | The unique Id of the AWS Greengrass Group
ggGroupId :: Lens' GetGroup Text
ggGroupId = lens _ggGroupId (\ s a -> s{_ggGroupId = a});

instance AWSRequest GetGroup where
        type Rs GetGroup = GetGroupResponse
        request = get greengrass
        response
          = receiveJSON
              (\ s h x ->
                 GetGroupResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetGroup

instance NFData GetGroup

instance ToHeaders GetGroup where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath GetGroup where
        toPath GetGroup'{..}
          = mconcat ["/greengrass/groups/", toBS _ggGroupId]

instance ToQuery GetGroup where
        toQuery = const mempty

-- | /See:/ 'getGroupResponse' smart constructor.
data GetGroupResponse = GetGroupResponse'
    { _ggrsLatestVersionARN     :: !(Maybe Text)
    , _ggrsARN                  :: !(Maybe Text)
    , _ggrsName                 :: !(Maybe Text)
    , _ggrsCreationTimestamp    :: !(Maybe Text)
    , _ggrsId                   :: !(Maybe Text)
    , _ggrsLatestVersion        :: !(Maybe Text)
    , _ggrsLastUpdatedTimestamp :: !(Maybe Text)
    , _ggrsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ggrsLatestVersionARN' - Latest version arn of the definition.
--
-- * 'ggrsARN' - Arn of the definition.
--
-- * 'ggrsName' - Name of the definition.
--
-- * 'ggrsCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'ggrsId' - Id of the definition.
--
-- * 'ggrsLatestVersion' - Last version of the definition.
--
-- * 'ggrsLastUpdatedTimestamp' - Last updated timestamp of the definition.
--
-- * 'ggrsResponseStatus' - -- | The response status code.
getGroupResponse
    :: Int -- ^ 'ggrsResponseStatus'
    -> GetGroupResponse
getGroupResponse pResponseStatus_ =
    GetGroupResponse'
    { _ggrsLatestVersionARN = Nothing
    , _ggrsARN = Nothing
    , _ggrsName = Nothing
    , _ggrsCreationTimestamp = Nothing
    , _ggrsId = Nothing
    , _ggrsLatestVersion = Nothing
    , _ggrsLastUpdatedTimestamp = Nothing
    , _ggrsResponseStatus = pResponseStatus_
    }

-- | Latest version arn of the definition.
ggrsLatestVersionARN :: Lens' GetGroupResponse (Maybe Text)
ggrsLatestVersionARN = lens _ggrsLatestVersionARN (\ s a -> s{_ggrsLatestVersionARN = a});

-- | Arn of the definition.
ggrsARN :: Lens' GetGroupResponse (Maybe Text)
ggrsARN = lens _ggrsARN (\ s a -> s{_ggrsARN = a});

-- | Name of the definition.
ggrsName :: Lens' GetGroupResponse (Maybe Text)
ggrsName = lens _ggrsName (\ s a -> s{_ggrsName = a});

-- | Timestamp of when the definition was created.
ggrsCreationTimestamp :: Lens' GetGroupResponse (Maybe Text)
ggrsCreationTimestamp = lens _ggrsCreationTimestamp (\ s a -> s{_ggrsCreationTimestamp = a});

-- | Id of the definition.
ggrsId :: Lens' GetGroupResponse (Maybe Text)
ggrsId = lens _ggrsId (\ s a -> s{_ggrsId = a});

-- | Last version of the definition.
ggrsLatestVersion :: Lens' GetGroupResponse (Maybe Text)
ggrsLatestVersion = lens _ggrsLatestVersion (\ s a -> s{_ggrsLatestVersion = a});

-- | Last updated timestamp of the definition.
ggrsLastUpdatedTimestamp :: Lens' GetGroupResponse (Maybe Text)
ggrsLastUpdatedTimestamp = lens _ggrsLastUpdatedTimestamp (\ s a -> s{_ggrsLastUpdatedTimestamp = a});

-- | -- | The response status code.
ggrsResponseStatus :: Lens' GetGroupResponse Int
ggrsResponseStatus = lens _ggrsResponseStatus (\ s a -> s{_ggrsResponseStatus = a});

instance NFData GetGroupResponse

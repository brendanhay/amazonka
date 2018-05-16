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
-- Module      : Network.AWS.Greengrass.CreateGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group. You may provide the initial version of the group or use ''CreateGroupVersion'' at a later time.
module Network.AWS.Greengrass.CreateGroup
    (
    -- * Creating a Request
      createGroup
    , CreateGroup
    -- * Request Lenses
    , cgAmznClientToken
    , cgInitialVersion
    , cgName

    -- * Destructuring the Response
    , createGroupResponse
    , CreateGroupResponse
    -- * Response Lenses
    , cgrsLatestVersionARN
    , cgrsARN
    , cgrsName
    , cgrsCreationTimestamp
    , cgrsId
    , cgrsLatestVersion
    , cgrsLastUpdatedTimestamp
    , cgrsResponseStatus
    ) where

import Network.AWS.Greengrass.Types
import Network.AWS.Greengrass.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createGroup' smart constructor.
data CreateGroup = CreateGroup'
  { _cgAmznClientToken :: !(Maybe Text)
  , _cgInitialVersion  :: !(Maybe GroupVersion)
  , _cgName            :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgAmznClientToken' - A client token used to correlate requests and responses.
--
-- * 'cgInitialVersion' - Information about the initial version of the group.
--
-- * 'cgName' - The name of the group.
createGroup
    :: CreateGroup
createGroup =
  CreateGroup'
    { _cgAmznClientToken = Nothing
    , _cgInitialVersion = Nothing
    , _cgName = Nothing
    }


-- | A client token used to correlate requests and responses.
cgAmznClientToken :: Lens' CreateGroup (Maybe Text)
cgAmznClientToken = lens _cgAmznClientToken (\ s a -> s{_cgAmznClientToken = a})

-- | Information about the initial version of the group.
cgInitialVersion :: Lens' CreateGroup (Maybe GroupVersion)
cgInitialVersion = lens _cgInitialVersion (\ s a -> s{_cgInitialVersion = a})

-- | The name of the group.
cgName :: Lens' CreateGroup (Maybe Text)
cgName = lens _cgName (\ s a -> s{_cgName = a})

instance AWSRequest CreateGroup where
        type Rs CreateGroup = CreateGroupResponse
        request = postJSON greengrass
        response
          = receiveJSON
              (\ s h x ->
                 CreateGroupResponse' <$>
                   (x .?> "LatestVersionArn") <*> (x .?> "Arn") <*>
                     (x .?> "Name")
                     <*> (x .?> "CreationTimestamp")
                     <*> (x .?> "Id")
                     <*> (x .?> "LatestVersion")
                     <*> (x .?> "LastUpdatedTimestamp")
                     <*> (pure (fromEnum s)))

instance Hashable CreateGroup where

instance NFData CreateGroup where

instance ToHeaders CreateGroup where
        toHeaders CreateGroup'{..}
          = mconcat
              ["X-Amzn-Client-Token" =# _cgAmznClientToken,
               "Content-Type" =#
                 ("application/x-amz-json-1.1" :: ByteString)]

instance ToJSON CreateGroup where
        toJSON CreateGroup'{..}
          = object
              (catMaybes
                 [("InitialVersion" .=) <$> _cgInitialVersion,
                  ("Name" .=) <$> _cgName])

instance ToPath CreateGroup where
        toPath = const "/greengrass/groups"

instance ToQuery CreateGroup where
        toQuery = const mempty

-- | /See:/ 'createGroupResponse' smart constructor.
data CreateGroupResponse = CreateGroupResponse'
  { _cgrsLatestVersionARN     :: !(Maybe Text)
  , _cgrsARN                  :: !(Maybe Text)
  , _cgrsName                 :: !(Maybe Text)
  , _cgrsCreationTimestamp    :: !(Maybe Text)
  , _cgrsId                   :: !(Maybe Text)
  , _cgrsLatestVersion        :: !(Maybe Text)
  , _cgrsLastUpdatedTimestamp :: !(Maybe Text)
  , _cgrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cgrsLatestVersionARN' - The ARN of the latest version of the definition.
--
-- * 'cgrsARN' - The ARN of the definition.
--
-- * 'cgrsName' - The name of the definition.
--
-- * 'cgrsCreationTimestamp' - The time, in milliseconds since the epoch, when the definition was created.
--
-- * 'cgrsId' - The ID of the definition.
--
-- * 'cgrsLatestVersion' - The latest version of the definition.
--
-- * 'cgrsLastUpdatedTimestamp' - The time, in milliseconds since the epoch, when the definition was last updated.
--
-- * 'cgrsResponseStatus' - -- | The response status code.
createGroupResponse
    :: Int -- ^ 'cgrsResponseStatus'
    -> CreateGroupResponse
createGroupResponse pResponseStatus_ =
  CreateGroupResponse'
    { _cgrsLatestVersionARN = Nothing
    , _cgrsARN = Nothing
    , _cgrsName = Nothing
    , _cgrsCreationTimestamp = Nothing
    , _cgrsId = Nothing
    , _cgrsLatestVersion = Nothing
    , _cgrsLastUpdatedTimestamp = Nothing
    , _cgrsResponseStatus = pResponseStatus_
    }


-- | The ARN of the latest version of the definition.
cgrsLatestVersionARN :: Lens' CreateGroupResponse (Maybe Text)
cgrsLatestVersionARN = lens _cgrsLatestVersionARN (\ s a -> s{_cgrsLatestVersionARN = a})

-- | The ARN of the definition.
cgrsARN :: Lens' CreateGroupResponse (Maybe Text)
cgrsARN = lens _cgrsARN (\ s a -> s{_cgrsARN = a})

-- | The name of the definition.
cgrsName :: Lens' CreateGroupResponse (Maybe Text)
cgrsName = lens _cgrsName (\ s a -> s{_cgrsName = a})

-- | The time, in milliseconds since the epoch, when the definition was created.
cgrsCreationTimestamp :: Lens' CreateGroupResponse (Maybe Text)
cgrsCreationTimestamp = lens _cgrsCreationTimestamp (\ s a -> s{_cgrsCreationTimestamp = a})

-- | The ID of the definition.
cgrsId :: Lens' CreateGroupResponse (Maybe Text)
cgrsId = lens _cgrsId (\ s a -> s{_cgrsId = a})

-- | The latest version of the definition.
cgrsLatestVersion :: Lens' CreateGroupResponse (Maybe Text)
cgrsLatestVersion = lens _cgrsLatestVersion (\ s a -> s{_cgrsLatestVersion = a})

-- | The time, in milliseconds since the epoch, when the definition was last updated.
cgrsLastUpdatedTimestamp :: Lens' CreateGroupResponse (Maybe Text)
cgrsLastUpdatedTimestamp = lens _cgrsLastUpdatedTimestamp (\ s a -> s{_cgrsLastUpdatedTimestamp = a})

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a})

instance NFData CreateGroupResponse where

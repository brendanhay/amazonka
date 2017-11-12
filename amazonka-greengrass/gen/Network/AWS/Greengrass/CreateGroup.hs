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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a group. You may optionally provide the initial version of the group or use ''CreateGroupVersion'' at a later time.
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
-- * 'cgAmznClientToken' - The client token used to request idempotent operations.
--
-- * 'cgInitialVersion' - Information on the initial version
--
-- * 'cgName' - name of the group
createGroup
    :: CreateGroup
createGroup =
  CreateGroup'
  {_cgAmznClientToken = Nothing, _cgInitialVersion = Nothing, _cgName = Nothing}


-- | The client token used to request idempotent operations.
cgAmznClientToken :: Lens' CreateGroup (Maybe Text)
cgAmznClientToken = lens _cgAmznClientToken (\ s a -> s{_cgAmznClientToken = a});

-- | Information on the initial version
cgInitialVersion :: Lens' CreateGroup (Maybe GroupVersion)
cgInitialVersion = lens _cgInitialVersion (\ s a -> s{_cgInitialVersion = a});

-- | name of the group
cgName :: Lens' CreateGroup (Maybe Text)
cgName = lens _cgName (\ s a -> s{_cgName = a});

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
-- * 'cgrsLatestVersionARN' - Latest version arn of the definition.
--
-- * 'cgrsARN' - Arn of the definition.
--
-- * 'cgrsName' - Name of the definition.
--
-- * 'cgrsCreationTimestamp' - Timestamp of when the definition was created.
--
-- * 'cgrsId' - Id of the definition.
--
-- * 'cgrsLatestVersion' - Last version of the definition.
--
-- * 'cgrsLastUpdatedTimestamp' - Last updated timestamp of the definition.
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


-- | Latest version arn of the definition.
cgrsLatestVersionARN :: Lens' CreateGroupResponse (Maybe Text)
cgrsLatestVersionARN = lens _cgrsLatestVersionARN (\ s a -> s{_cgrsLatestVersionARN = a});

-- | Arn of the definition.
cgrsARN :: Lens' CreateGroupResponse (Maybe Text)
cgrsARN = lens _cgrsARN (\ s a -> s{_cgrsARN = a});

-- | Name of the definition.
cgrsName :: Lens' CreateGroupResponse (Maybe Text)
cgrsName = lens _cgrsName (\ s a -> s{_cgrsName = a});

-- | Timestamp of when the definition was created.
cgrsCreationTimestamp :: Lens' CreateGroupResponse (Maybe Text)
cgrsCreationTimestamp = lens _cgrsCreationTimestamp (\ s a -> s{_cgrsCreationTimestamp = a});

-- | Id of the definition.
cgrsId :: Lens' CreateGroupResponse (Maybe Text)
cgrsId = lens _cgrsId (\ s a -> s{_cgrsId = a});

-- | Last version of the definition.
cgrsLatestVersion :: Lens' CreateGroupResponse (Maybe Text)
cgrsLatestVersion = lens _cgrsLatestVersion (\ s a -> s{_cgrsLatestVersion = a});

-- | Last updated timestamp of the definition.
cgrsLastUpdatedTimestamp :: Lens' CreateGroupResponse (Maybe Text)
cgrsLastUpdatedTimestamp = lens _cgrsLastUpdatedTimestamp (\ s a -> s{_cgrsLastUpdatedTimestamp = a});

-- | -- | The response status code.
cgrsResponseStatus :: Lens' CreateGroupResponse Int
cgrsResponseStatus = lens _cgrsResponseStatus (\ s a -> s{_cgrsResponseStatus = a});

instance NFData CreateGroupResponse where

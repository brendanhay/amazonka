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
-- Module      : Network.AWS.EC2.GetConsoleOutput
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the console output for the specified instance.
--
--
-- Instances do not have a physical monitor through which you can view their console output. They also lack physical controls that allow you to power up, reboot, or shut them down. To allow these actions, we provide them through the Amazon EC2 API and command line interface.
--
-- Instance console output is buffered and posted shortly after instance boot, reboot, and termination. Amazon EC2 preserves the most recent 64 KB output, which is available for at least one hour after the most recent post.
--
-- For Linux instances, the instance console output displays the exact console output that would normally be displayed on a physical monitor attached to a computer. This output is buffered because the instance produces it and then posts it to a store where the instance's owner can retrieve it.
--
-- For Windows instances, the instance console output includes output from the EC2Config service.
--
module Network.AWS.EC2.GetConsoleOutput
    (
    -- * Creating a Request
      getConsoleOutput
    , GetConsoleOutput
    -- * Request Lenses
    , gcoDryRun
    , gcoInstanceId

    -- * Destructuring the Response
    , getConsoleOutputResponse
    , GetConsoleOutputResponse
    -- * Response Lenses
    , gcorsInstanceId
    , gcorsOutput
    , gcorsTimestamp
    , gcorsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for GetConsoleOutput.
--
--
--
-- /See:/ 'getConsoleOutput' smart constructor.
data GetConsoleOutput = GetConsoleOutput'
  { _gcoDryRun     :: !(Maybe Bool)
  , _gcoInstanceId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConsoleOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcoDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gcoInstanceId' - The ID of the instance.
getConsoleOutput
    :: Text -- ^ 'gcoInstanceId'
    -> GetConsoleOutput
getConsoleOutput pInstanceId_ =
  GetConsoleOutput' {_gcoDryRun = Nothing, _gcoInstanceId = pInstanceId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gcoDryRun :: Lens' GetConsoleOutput (Maybe Bool)
gcoDryRun = lens _gcoDryRun (\ s a -> s{_gcoDryRun = a});

-- | The ID of the instance.
gcoInstanceId :: Lens' GetConsoleOutput Text
gcoInstanceId = lens _gcoInstanceId (\ s a -> s{_gcoInstanceId = a});

instance AWSRequest GetConsoleOutput where
        type Rs GetConsoleOutput = GetConsoleOutputResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetConsoleOutputResponse' <$>
                   (x .@? "instanceId") <*> (x .@? "output") <*>
                     (x .@? "timestamp")
                     <*> (pure (fromEnum s)))

instance Hashable GetConsoleOutput where

instance NFData GetConsoleOutput where

instance ToHeaders GetConsoleOutput where
        toHeaders = const mempty

instance ToPath GetConsoleOutput where
        toPath = const "/"

instance ToQuery GetConsoleOutput where
        toQuery GetConsoleOutput'{..}
          = mconcat
              ["Action" =: ("GetConsoleOutput" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _gcoDryRun,
               "InstanceId" =: _gcoInstanceId]

-- | Contains the output of GetConsoleOutput.
--
--
--
-- /See:/ 'getConsoleOutputResponse' smart constructor.
data GetConsoleOutputResponse = GetConsoleOutputResponse'
  { _gcorsInstanceId     :: !(Maybe Text)
  , _gcorsOutput         :: !(Maybe Text)
  , _gcorsTimestamp      :: !(Maybe ISO8601)
  , _gcorsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetConsoleOutputResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcorsInstanceId' - The ID of the instance.
--
-- * 'gcorsOutput' - The console output, Base64-encoded. If using a command line tool, the tool decodes the output for you.
--
-- * 'gcorsTimestamp' - The time the output was last updated.
--
-- * 'gcorsResponseStatus' - -- | The response status code.
getConsoleOutputResponse
    :: Int -- ^ 'gcorsResponseStatus'
    -> GetConsoleOutputResponse
getConsoleOutputResponse pResponseStatus_ =
  GetConsoleOutputResponse'
  { _gcorsInstanceId = Nothing
  , _gcorsOutput = Nothing
  , _gcorsTimestamp = Nothing
  , _gcorsResponseStatus = pResponseStatus_
  }


-- | The ID of the instance.
gcorsInstanceId :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorsInstanceId = lens _gcorsInstanceId (\ s a -> s{_gcorsInstanceId = a});

-- | The console output, Base64-encoded. If using a command line tool, the tool decodes the output for you.
gcorsOutput :: Lens' GetConsoleOutputResponse (Maybe Text)
gcorsOutput = lens _gcorsOutput (\ s a -> s{_gcorsOutput = a});

-- | The time the output was last updated.
gcorsTimestamp :: Lens' GetConsoleOutputResponse (Maybe UTCTime)
gcorsTimestamp = lens _gcorsTimestamp (\ s a -> s{_gcorsTimestamp = a}) . mapping _Time;

-- | -- | The response status code.
gcorsResponseStatus :: Lens' GetConsoleOutputResponse Int
gcorsResponseStatus = lens _gcorsResponseStatus (\ s a -> s{_gcorsResponseStatus = a});

instance NFData GetConsoleOutputResponse where

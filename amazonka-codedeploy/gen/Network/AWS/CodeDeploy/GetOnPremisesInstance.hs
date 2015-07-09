{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.GetOnPremisesInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an on-premises instance.
--
-- <http://docs.aws.amazon.com/codedeploy/latest/APIReference/API_GetOnPremisesInstance.html>
module Network.AWS.CodeDeploy.GetOnPremisesInstance
    (
    -- * Request
      GetOnPremisesInstance
    -- ** Request constructor
    , getOnPremisesInstance
    -- ** Request lenses
    , gopiInstanceName

    -- * Response
    , GetOnPremisesInstanceResponse
    -- ** Response constructor
    , getOnPremisesInstanceResponse
    -- ** Response lenses
    , gopirInstanceInfo
    , gopirStatus
    ) where

import           Network.AWS.CodeDeploy.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a get on-premises instance operation.
--
-- /See:/ 'getOnPremisesInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopiInstanceName'
newtype GetOnPremisesInstance = GetOnPremisesInstance'
    { _gopiInstanceName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOnPremisesInstance' smart constructor.
getOnPremisesInstance :: Text -> GetOnPremisesInstance
getOnPremisesInstance pInstanceName =
    GetOnPremisesInstance'
    { _gopiInstanceName = pInstanceName
    }

-- | The name of the on-premises instance to get information about
gopiInstanceName :: Lens' GetOnPremisesInstance Text
gopiInstanceName = lens _gopiInstanceName (\ s a -> s{_gopiInstanceName = a});

instance AWSRequest GetOnPremisesInstance where
        type Sv GetOnPremisesInstance = CodeDeploy
        type Rs GetOnPremisesInstance =
             GetOnPremisesInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 GetOnPremisesInstanceResponse' <$>
                   (x .?> "instanceInfo") <*> (pure (fromEnum s)))

instance ToHeaders GetOnPremisesInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeDeploy_20141006.GetOnPremisesInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetOnPremisesInstance where
        toJSON GetOnPremisesInstance'{..}
          = object ["instanceName" .= _gopiInstanceName]

instance ToPath GetOnPremisesInstance where
        toPath = const "/"

instance ToQuery GetOnPremisesInstance where
        toQuery = const mempty

-- | Represents the output of a get on-premises instance operation.
--
-- /See:/ 'getOnPremisesInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gopirInstanceInfo'
--
-- * 'gopirStatus'
data GetOnPremisesInstanceResponse = GetOnPremisesInstanceResponse'
    { _gopirInstanceInfo :: !(Maybe InstanceInfo)
    , _gopirStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetOnPremisesInstanceResponse' smart constructor.
getOnPremisesInstanceResponse :: Int -> GetOnPremisesInstanceResponse
getOnPremisesInstanceResponse pStatus =
    GetOnPremisesInstanceResponse'
    { _gopirInstanceInfo = Nothing
    , _gopirStatus = pStatus
    }

-- | Information about the on-premises instance.
gopirInstanceInfo :: Lens' GetOnPremisesInstanceResponse (Maybe InstanceInfo)
gopirInstanceInfo = lens _gopirInstanceInfo (\ s a -> s{_gopirInstanceInfo = a});

-- | FIXME: Undocumented member.
gopirStatus :: Lens' GetOnPremisesInstanceResponse Int
gopirStatus = lens _gopirStatus (\ s a -> s{_gopirStatus = a});

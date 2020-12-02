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
-- Module      : Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates a request to compile the specified type of information of the deployed environment.
--
--
-- Setting the @InfoType@ to @tail@ compiles the last lines from the application server log files of every Amazon EC2 instance in your environment.
--
-- Setting the @InfoType@ to @bundle@ compresses the application server log files for every Amazon EC2 instance into a @.zip@ file. Legacy and .NET containers do not support bundle logs.
--
-- Use 'RetrieveEnvironmentInfo' to obtain the set of logs.
--
-- Related Topics
--
--     * 'RetrieveEnvironmentInfo'
--
--
--
module Network.AWS.ElasticBeanstalk.RequestEnvironmentInfo
    (
    -- * Creating a Request
      requestEnvironmentInfo
    , RequestEnvironmentInfo
    -- * Request Lenses
    , reiEnvironmentName
    , reiEnvironmentId
    , reiInfoType

    -- * Destructuring the Response
    , requestEnvironmentInfoResponse
    , RequestEnvironmentInfoResponse
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to retrieve logs from an environment and store them in your Elastic Beanstalk storage bucket.
--
--
--
-- /See:/ 'requestEnvironmentInfo' smart constructor.
data RequestEnvironmentInfo = RequestEnvironmentInfo'
  { _reiEnvironmentName :: !(Maybe Text)
  , _reiEnvironmentId   :: !(Maybe Text)
  , _reiInfoType        :: !EnvironmentInfoType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestEnvironmentInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'reiEnvironmentName' - The name of the environment of the requested data. If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'reiEnvironmentId' - The ID of the environment of the requested data. If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
--
-- * 'reiInfoType' - The type of information to request.
requestEnvironmentInfo
    :: EnvironmentInfoType -- ^ 'reiInfoType'
    -> RequestEnvironmentInfo
requestEnvironmentInfo pInfoType_ =
  RequestEnvironmentInfo'
    { _reiEnvironmentName = Nothing
    , _reiEnvironmentId = Nothing
    , _reiInfoType = pInfoType_
    }


-- | The name of the environment of the requested data. If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentId, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
reiEnvironmentName :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentName = lens _reiEnvironmentName (\ s a -> s{_reiEnvironmentName = a})

-- | The ID of the environment of the requested data. If no such environment is found, @RequestEnvironmentInfo@ returns an @InvalidParameterValue@ error.  Condition: You must specify either this or an EnvironmentName, or both. If you do not specify either, AWS Elastic Beanstalk returns @MissingRequiredParameter@ error.
reiEnvironmentId :: Lens' RequestEnvironmentInfo (Maybe Text)
reiEnvironmentId = lens _reiEnvironmentId (\ s a -> s{_reiEnvironmentId = a})

-- | The type of information to request.
reiInfoType :: Lens' RequestEnvironmentInfo EnvironmentInfoType
reiInfoType = lens _reiInfoType (\ s a -> s{_reiInfoType = a})

instance AWSRequest RequestEnvironmentInfo where
        type Rs RequestEnvironmentInfo =
             RequestEnvironmentInfoResponse
        request = postQuery elasticBeanstalk
        response
          = receiveNull RequestEnvironmentInfoResponse'

instance Hashable RequestEnvironmentInfo where

instance NFData RequestEnvironmentInfo where

instance ToHeaders RequestEnvironmentInfo where
        toHeaders = const mempty

instance ToPath RequestEnvironmentInfo where
        toPath = const "/"

instance ToQuery RequestEnvironmentInfo where
        toQuery RequestEnvironmentInfo'{..}
          = mconcat
              ["Action" =:
                 ("RequestEnvironmentInfo" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "EnvironmentName" =: _reiEnvironmentName,
               "EnvironmentId" =: _reiEnvironmentId,
               "InfoType" =: _reiInfoType]

-- | /See:/ 'requestEnvironmentInfoResponse' smart constructor.
data RequestEnvironmentInfoResponse =
  RequestEnvironmentInfoResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RequestEnvironmentInfoResponse' with the minimum fields required to make a request.
--
requestEnvironmentInfoResponse
    :: RequestEnvironmentInfoResponse
requestEnvironmentInfoResponse = RequestEnvironmentInfoResponse'


instance NFData RequestEnvironmentInfoResponse where

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
-- Module      : Network.AWS.EMR.DescribeSecurityConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the details of a security configuration by returning the configuration JSON.
--
--
module Network.AWS.EMR.DescribeSecurityConfiguration
    (
    -- * Creating a Request
      describeSecurityConfiguration
    , DescribeSecurityConfiguration
    -- * Request Lenses
    , dName

    -- * Destructuring the Response
    , describeSecurityConfigurationResponse
    , DescribeSecurityConfigurationResponse
    -- * Response Lenses
    , drsSecurityConfiguration
    , drsName
    , drsCreationDateTime
    , drsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeSecurityConfiguration' smart constructor.
newtype DescribeSecurityConfiguration = DescribeSecurityConfiguration'
  { _dName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dName' - The name of the security configuration.
describeSecurityConfiguration
    :: Text -- ^ 'dName'
    -> DescribeSecurityConfiguration
describeSecurityConfiguration pName_ =
  DescribeSecurityConfiguration' {_dName = pName_}


-- | The name of the security configuration.
dName :: Lens' DescribeSecurityConfiguration Text
dName = lens _dName (\ s a -> s{_dName = a})

instance AWSRequest DescribeSecurityConfiguration
         where
        type Rs DescribeSecurityConfiguration =
             DescribeSecurityConfigurationResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 DescribeSecurityConfigurationResponse' <$>
                   (x .?> "SecurityConfiguration") <*> (x .?> "Name")
                     <*> (x .?> "CreationDateTime")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeSecurityConfiguration where

instance NFData DescribeSecurityConfiguration where

instance ToHeaders DescribeSecurityConfiguration
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.DescribeSecurityConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeSecurityConfiguration where
        toJSON DescribeSecurityConfiguration'{..}
          = object (catMaybes [Just ("Name" .= _dName)])

instance ToPath DescribeSecurityConfiguration where
        toPath = const "/"

instance ToQuery DescribeSecurityConfiguration where
        toQuery = const mempty

-- | /See:/ 'describeSecurityConfigurationResponse' smart constructor.
data DescribeSecurityConfigurationResponse = DescribeSecurityConfigurationResponse'
  { _drsSecurityConfiguration :: !(Maybe Text)
  , _drsName                  :: !(Maybe Text)
  , _drsCreationDateTime      :: !(Maybe POSIX)
  , _drsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsSecurityConfiguration' - The security configuration details in JSON format.
--
-- * 'drsName' - The name of the security configuration.
--
-- * 'drsCreationDateTime' - The date and time the security configuration was created
--
-- * 'drsResponseStatus' - -- | The response status code.
describeSecurityConfigurationResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeSecurityConfigurationResponse
describeSecurityConfigurationResponse pResponseStatus_ =
  DescribeSecurityConfigurationResponse'
    { _drsSecurityConfiguration = Nothing
    , _drsName = Nothing
    , _drsCreationDateTime = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The security configuration details in JSON format.
drsSecurityConfiguration :: Lens' DescribeSecurityConfigurationResponse (Maybe Text)
drsSecurityConfiguration = lens _drsSecurityConfiguration (\ s a -> s{_drsSecurityConfiguration = a})

-- | The name of the security configuration.
drsName :: Lens' DescribeSecurityConfigurationResponse (Maybe Text)
drsName = lens _drsName (\ s a -> s{_drsName = a})

-- | The date and time the security configuration was created
drsCreationDateTime :: Lens' DescribeSecurityConfigurationResponse (Maybe UTCTime)
drsCreationDateTime = lens _drsCreationDateTime (\ s a -> s{_drsCreationDateTime = a}) . mapping _Time

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeSecurityConfigurationResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DescribeSecurityConfigurationResponse
         where

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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's upcoming and in-progress managed actions.
--
--
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActions
    (
    -- * Creating a Request
      describeEnvironmentManagedActions
    , DescribeEnvironmentManagedActions
    -- * Request Lenses
    , demaStatus
    , demaEnvironmentName
    , demaEnvironmentId

    -- * Destructuring the Response
    , describeEnvironmentManagedActionsResponse
    , DescribeEnvironmentManagedActionsResponse
    -- * Response Lenses
    , demarsManagedActions
    , demarsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to list an environment's upcoming and in-progress managed actions.
--
--
--
-- /See:/ 'describeEnvironmentManagedActions' smart constructor.
data DescribeEnvironmentManagedActions = DescribeEnvironmentManagedActions'
  { _demaStatus          :: !(Maybe ActionStatus)
  , _demaEnvironmentName :: !(Maybe Text)
  , _demaEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentManagedActions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demaStatus' - To show only actions with a particular status, specify a status.
--
-- * 'demaEnvironmentName' - The name of the target environment.
--
-- * 'demaEnvironmentId' - The environment ID of the target environment.
describeEnvironmentManagedActions
    :: DescribeEnvironmentManagedActions
describeEnvironmentManagedActions =
  DescribeEnvironmentManagedActions'
    { _demaStatus = Nothing
    , _demaEnvironmentName = Nothing
    , _demaEnvironmentId = Nothing
    }


-- | To show only actions with a particular status, specify a status.
demaStatus :: Lens' DescribeEnvironmentManagedActions (Maybe ActionStatus)
demaStatus = lens _demaStatus (\ s a -> s{_demaStatus = a})

-- | The name of the target environment.
demaEnvironmentName :: Lens' DescribeEnvironmentManagedActions (Maybe Text)
demaEnvironmentName = lens _demaEnvironmentName (\ s a -> s{_demaEnvironmentName = a})

-- | The environment ID of the target environment.
demaEnvironmentId :: Lens' DescribeEnvironmentManagedActions (Maybe Text)
demaEnvironmentId = lens _demaEnvironmentId (\ s a -> s{_demaEnvironmentId = a})

instance AWSRequest DescribeEnvironmentManagedActions
         where
        type Rs DescribeEnvironmentManagedActions =
             DescribeEnvironmentManagedActionsResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "DescribeEnvironmentManagedActionsResult"
              (\ s h x ->
                 DescribeEnvironmentManagedActionsResponse' <$>
                   (x .@? "ManagedActions" .!@ mempty >>=
                      may (parseXMLList1 "member"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeEnvironmentManagedActions
         where

instance NFData DescribeEnvironmentManagedActions
         where

instance ToHeaders DescribeEnvironmentManagedActions
         where
        toHeaders = const mempty

instance ToPath DescribeEnvironmentManagedActions
         where
        toPath = const "/"

instance ToQuery DescribeEnvironmentManagedActions
         where
        toQuery DescribeEnvironmentManagedActions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEnvironmentManagedActions" :: ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "Status" =: _demaStatus,
               "EnvironmentName" =: _demaEnvironmentName,
               "EnvironmentId" =: _demaEnvironmentId]

-- | The result message containing a list of managed actions.
--
--
--
-- /See:/ 'describeEnvironmentManagedActionsResponse' smart constructor.
data DescribeEnvironmentManagedActionsResponse = DescribeEnvironmentManagedActionsResponse'
  { _demarsManagedActions :: !(Maybe (List1 ManagedAction))
  , _demarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentManagedActionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demarsManagedActions' - A list of upcoming and in-progress managed actions.
--
-- * 'demarsResponseStatus' - -- | The response status code.
describeEnvironmentManagedActionsResponse
    :: Int -- ^ 'demarsResponseStatus'
    -> DescribeEnvironmentManagedActionsResponse
describeEnvironmentManagedActionsResponse pResponseStatus_ =
  DescribeEnvironmentManagedActionsResponse'
    {_demarsManagedActions = Nothing, _demarsResponseStatus = pResponseStatus_}


-- | A list of upcoming and in-progress managed actions.
demarsManagedActions :: Lens' DescribeEnvironmentManagedActionsResponse (Maybe (NonEmpty ManagedAction))
demarsManagedActions = lens _demarsManagedActions (\ s a -> s{_demarsManagedActions = a}) . mapping _List1

-- | -- | The response status code.
demarsResponseStatus :: Lens' DescribeEnvironmentManagedActionsResponse Int
demarsResponseStatus = lens _demarsResponseStatus (\ s a -> s{_demarsResponseStatus = a})

instance NFData
           DescribeEnvironmentManagedActionsResponse
         where

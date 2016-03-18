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
-- Module      : Network.AWS.SSM.DescribeAssociation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations for the specified SSM document or instance.
module Network.AWS.SSM.DescribeAssociation
    (
    -- * Creating a Request
      describeAssociation
    , DescribeAssociation
    -- * Request Lenses
    , daName
    , daInstanceId

    -- * Destructuring the Response
    , describeAssociationResponse
    , DescribeAssociationResponse
    -- * Response Lenses
    , darsAssociationDescription
    , darsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
    { _daName       :: !Text
    , _daInstanceId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daName'
--
-- * 'daInstanceId'
describeAssociation
    :: Text -- ^ 'daName'
    -> Text -- ^ 'daInstanceId'
    -> DescribeAssociation
describeAssociation pName_ pInstanceId_ =
    DescribeAssociation'
    { _daName = pName_
    , _daInstanceId = pInstanceId_
    }

-- | The name of the SSM document.
daName :: Lens' DescribeAssociation Text
daName = lens _daName (\ s a -> s{_daName = a});

-- | The Windows Server instance ID.
daInstanceId :: Lens' DescribeAssociation Text
daInstanceId = lens _daInstanceId (\ s a -> s{_daInstanceId = a});

instance AWSRequest DescribeAssociation where
        type Rs DescribeAssociation =
             DescribeAssociationResponse
        request = postJSON sSM
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance ToHeaders DescribeAssociation where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DescribeAssociation" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeAssociation where
        toJSON DescribeAssociation'{..}
          = object
              (catMaybes
                 [Just ("Name" .= _daName),
                  Just ("InstanceId" .= _daInstanceId)])

instance ToPath DescribeAssociation where
        toPath = const "/"

instance ToQuery DescribeAssociation where
        toQuery = const mempty

-- | /See:/ 'describeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
    { _darsAssociationDescription :: !(Maybe AssociationDescription)
    , _darsResponseStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsAssociationDescription'
--
-- * 'darsResponseStatus'
describeAssociationResponse
    :: Int -- ^ 'darsResponseStatus'
    -> DescribeAssociationResponse
describeAssociationResponse pResponseStatus_ =
    DescribeAssociationResponse'
    { _darsAssociationDescription = Nothing
    , _darsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
darsAssociationDescription :: Lens' DescribeAssociationResponse (Maybe AssociationDescription)
darsAssociationDescription = lens _darsAssociationDescription (\ s a -> s{_darsAssociationDescription = a});

-- | The response status code.
darsResponseStatus :: Lens' DescribeAssociationResponse Int
darsResponseStatus = lens _darsResponseStatus (\ s a -> s{_darsResponseStatus = a});

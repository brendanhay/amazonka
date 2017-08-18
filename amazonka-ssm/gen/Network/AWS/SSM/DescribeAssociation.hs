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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the associations for the specified Systems Manager document or instance.
--
--
module Network.AWS.SSM.DescribeAssociation
    (
    -- * Creating a Request
      describeAssociation
    , DescribeAssociation
    -- * Request Lenses
    , daAssociationId
    , daInstanceId
    , daName

    -- * Destructuring the Response
    , describeAssociationResponse
    , DescribeAssociationResponse
    -- * Response Lenses
    , daarsAssociationDescription
    , daarsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SSM.Types
import           Network.AWS.SSM.Types.Product

-- | /See:/ 'describeAssociation' smart constructor.
data DescribeAssociation = DescribeAssociation'
    { _daAssociationId :: !(Maybe Text)
    , _daInstanceId    :: !(Maybe Text)
    , _daName          :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssociation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAssociationId' - The association ID for which you want information.
--
-- * 'daInstanceId' - The instance ID.
--
-- * 'daName' - The name of the SSM document.
describeAssociation
    :: DescribeAssociation
describeAssociation =
    DescribeAssociation'
    { _daAssociationId = Nothing
    , _daInstanceId = Nothing
    , _daName = Nothing
    }

-- | The association ID for which you want information.
daAssociationId :: Lens' DescribeAssociation (Maybe Text)
daAssociationId = lens _daAssociationId (\ s a -> s{_daAssociationId = a});

-- | The instance ID.
daInstanceId :: Lens' DescribeAssociation (Maybe Text)
daInstanceId = lens _daInstanceId (\ s a -> s{_daInstanceId = a});

-- | The name of the SSM document.
daName :: Lens' DescribeAssociation (Maybe Text)
daName = lens _daName (\ s a -> s{_daName = a});

instance AWSRequest DescribeAssociation where
        type Rs DescribeAssociation =
             DescribeAssociationResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DescribeAssociationResponse' <$>
                   (x .?> "AssociationDescription") <*>
                     (pure (fromEnum s)))

instance Hashable DescribeAssociation

instance NFData DescribeAssociation

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
                 [("AssociationId" .=) <$> _daAssociationId,
                  ("InstanceId" .=) <$> _daInstanceId,
                  ("Name" .=) <$> _daName])

instance ToPath DescribeAssociation where
        toPath = const "/"

instance ToQuery DescribeAssociation where
        toQuery = const mempty

-- | /See:/ 'describeAssociationResponse' smart constructor.
data DescribeAssociationResponse = DescribeAssociationResponse'
    { _daarsAssociationDescription :: !(Maybe AssociationDescription)
    , _daarsResponseStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAssociationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daarsAssociationDescription' - Information about the association.
--
-- * 'daarsResponseStatus' - -- | The response status code.
describeAssociationResponse
    :: Int -- ^ 'daarsResponseStatus'
    -> DescribeAssociationResponse
describeAssociationResponse pResponseStatus_ =
    DescribeAssociationResponse'
    { _daarsAssociationDescription = Nothing
    , _daarsResponseStatus = pResponseStatus_
    }

-- | Information about the association.
daarsAssociationDescription :: Lens' DescribeAssociationResponse (Maybe AssociationDescription)
daarsAssociationDescription = lens _daarsAssociationDescription (\ s a -> s{_daarsAssociationDescription = a});

-- | -- | The response status code.
daarsResponseStatus :: Lens' DescribeAssociationResponse Int
daarsResponseStatus = lens _daarsResponseStatus (\ s a -> s{_daarsResponseStatus = a});

instance NFData DescribeAssociationResponse

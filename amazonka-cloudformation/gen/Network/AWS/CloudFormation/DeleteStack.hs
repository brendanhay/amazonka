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
-- Module      : Network.AWS.CloudFormation.DeleteStack
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified stack. Once the call completes successfully, stack deletion starts. Deleted stacks do not show up in the < DescribeStacks> API if the deletion has been completed successfully.
module Network.AWS.CloudFormation.DeleteStack
    (
    -- * Creating a Request
      deleteStack
    , DeleteStack
    -- * Request Lenses
    , dsRetainResources
    , dsStackName

    -- * Destructuring the Response
    , deleteStackResponse
    , DeleteStackResponse
    ) where

import           Network.AWS.CloudFormation.Types
import           Network.AWS.CloudFormation.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for < DeleteStack> action.
--
-- /See:/ 'deleteStack' smart constructor.
data DeleteStack = DeleteStack'
    { _dsRetainResources :: !(Maybe [Text])
    , _dsStackName       :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteStack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsRetainResources'
--
-- * 'dsStackName'
deleteStack
    :: Text -- ^ 'dsStackName'
    -> DeleteStack
deleteStack pStackName_ =
    DeleteStack'
    { _dsRetainResources = Nothing
    , _dsStackName = pStackName_
    }

-- | For stacks in the 'DELETE_FAILED' state, a list of resource logical IDs that are associated with the resources you want to retain. During deletion, AWS CloudFormation deletes the stack but does not delete the retained resources.
--
-- Retaining resources is useful when you cannot delete a resource, such as a non-empty S3 bucket, but you want to delete the stack.
dsRetainResources :: Lens' DeleteStack [Text]
dsRetainResources = lens _dsRetainResources (\ s a -> s{_dsRetainResources = a}) . _Default . _Coerce;

-- | The name or the unique stack ID that is associated with the stack.
dsStackName :: Lens' DeleteStack Text
dsStackName = lens _dsStackName (\ s a -> s{_dsStackName = a});

instance AWSRequest DeleteStack where
        type Rs DeleteStack = DeleteStackResponse
        request = postQuery cloudFormation
        response = receiveNull DeleteStackResponse'

instance Hashable DeleteStack

instance NFData DeleteStack

instance ToHeaders DeleteStack where
        toHeaders = const mempty

instance ToPath DeleteStack where
        toPath = const "/"

instance ToQuery DeleteStack where
        toQuery DeleteStack'{..}
          = mconcat
              ["Action" =: ("DeleteStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "RetainResources" =:
                 toQuery
                   (toQueryList "member" <$> _dsRetainResources),
               "StackName" =: _dsStackName]

-- | /See:/ 'deleteStackResponse' smart constructor.
data DeleteStackResponse =
    DeleteStackResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteStackResponse' with the minimum fields required to make a request.
--
deleteStackResponse
    :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse'

instance NFData DeleteStackResponse

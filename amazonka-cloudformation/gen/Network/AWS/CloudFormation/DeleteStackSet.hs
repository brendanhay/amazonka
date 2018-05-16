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
-- Module      : Network.AWS.CloudFormation.DeleteStackSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a stack set. Before you can delete a stack set, all of its member stack instances must be deleted. For more information about how to do this, see 'DeleteStackInstances' .
--
--
module Network.AWS.CloudFormation.DeleteStackSet
    (
    -- * Creating a Request
      deleteStackSet
    , DeleteStackSet
    -- * Request Lenses
    , dssStackSetName

    -- * Destructuring the Response
    , deleteStackSetResponse
    , DeleteStackSetResponse
    -- * Response Lenses
    , dssrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStackSet' smart constructor.
newtype DeleteStackSet = DeleteStackSet'
  { _dssStackSetName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssStackSetName' - The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
deleteStackSet
    :: Text -- ^ 'dssStackSetName'
    -> DeleteStackSet
deleteStackSet pStackSetName_ =
  DeleteStackSet' {_dssStackSetName = pStackSetName_}


-- | The name or unique ID of the stack set that you're deleting. You can obtain this value by running 'ListStackSets' .
dssStackSetName :: Lens' DeleteStackSet Text
dssStackSetName = lens _dssStackSetName (\ s a -> s{_dssStackSetName = a})

instance AWSRequest DeleteStackSet where
        type Rs DeleteStackSet = DeleteStackSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DeleteStackSetResult"
              (\ s h x ->
                 DeleteStackSetResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteStackSet where

instance NFData DeleteStackSet where

instance ToHeaders DeleteStackSet where
        toHeaders = const mempty

instance ToPath DeleteStackSet where
        toPath = const "/"

instance ToQuery DeleteStackSet where
        toQuery DeleteStackSet'{..}
          = mconcat
              ["Action" =: ("DeleteStackSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackSetName" =: _dssStackSetName]

-- | /See:/ 'deleteStackSetResponse' smart constructor.
newtype DeleteStackSetResponse = DeleteStackSetResponse'
  { _dssrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteStackSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dssrsResponseStatus' - -- | The response status code.
deleteStackSetResponse
    :: Int -- ^ 'dssrsResponseStatus'
    -> DeleteStackSetResponse
deleteStackSetResponse pResponseStatus_ =
  DeleteStackSetResponse' {_dssrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dssrsResponseStatus :: Lens' DeleteStackSetResponse Int
dssrsResponseStatus = lens _dssrsResponseStatus (\ s a -> s{_dssrsResponseStatus = a})

instance NFData DeleteStackSetResponse where

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
-- Module      : Network.AWS.CloudFormation.DeleteChangeSet
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified change set. Deleting change sets ensures that no one executes the wrong change set.
--
--
-- If the call successfully completes, AWS CloudFormation successfully deleted the change set.
--
module Network.AWS.CloudFormation.DeleteChangeSet
    (
    -- * Creating a Request
      deleteChangeSet
    , DeleteChangeSet
    -- * Request Lenses
    , dcsStackName
    , dcsChangeSetName

    -- * Destructuring the Response
    , deleteChangeSetResponse
    , DeleteChangeSetResponse
    -- * Response Lenses
    , dcsrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'DeleteChangeSet' action.
--
--
--
-- /See:/ 'deleteChangeSet' smart constructor.
data DeleteChangeSet = DeleteChangeSet'
  { _dcsStackName     :: !(Maybe Text)
  , _dcsChangeSetName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChangeSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsStackName' - If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
--
-- * 'dcsChangeSetName' - The name or Amazon Resource Name (ARN) of the change set that you want to delete.
deleteChangeSet
    :: Text -- ^ 'dcsChangeSetName'
    -> DeleteChangeSet
deleteChangeSet pChangeSetName_ =
  DeleteChangeSet'
    {_dcsStackName = Nothing, _dcsChangeSetName = pChangeSetName_}


-- | If you specified the name of a change set to delete, specify the stack name or ID (ARN) that is associated with it.
dcsStackName :: Lens' DeleteChangeSet (Maybe Text)
dcsStackName = lens _dcsStackName (\ s a -> s{_dcsStackName = a})

-- | The name or Amazon Resource Name (ARN) of the change set that you want to delete.
dcsChangeSetName :: Lens' DeleteChangeSet Text
dcsChangeSetName = lens _dcsChangeSetName (\ s a -> s{_dcsChangeSetName = a})

instance AWSRequest DeleteChangeSet where
        type Rs DeleteChangeSet = DeleteChangeSetResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "DeleteChangeSetResult"
              (\ s h x ->
                 DeleteChangeSetResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteChangeSet where

instance NFData DeleteChangeSet where

instance ToHeaders DeleteChangeSet where
        toHeaders = const mempty

instance ToPath DeleteChangeSet where
        toPath = const "/"

instance ToQuery DeleteChangeSet where
        toQuery DeleteChangeSet'{..}
          = mconcat
              ["Action" =: ("DeleteChangeSet" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _dcsStackName,
               "ChangeSetName" =: _dcsChangeSetName]

-- | The output for the 'DeleteChangeSet' action.
--
--
--
-- /See:/ 'deleteChangeSetResponse' smart constructor.
newtype DeleteChangeSetResponse = DeleteChangeSetResponse'
  { _dcsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteChangeSetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsrsResponseStatus' - -- | The response status code.
deleteChangeSetResponse
    :: Int -- ^ 'dcsrsResponseStatus'
    -> DeleteChangeSetResponse
deleteChangeSetResponse pResponseStatus_ =
  DeleteChangeSetResponse' {_dcsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dcsrsResponseStatus :: Lens' DeleteChangeSetResponse Int
dcsrsResponseStatus = lens _dcsrsResponseStatus (\ s a -> s{_dcsrsResponseStatus = a})

instance NFData DeleteChangeSetResponse where

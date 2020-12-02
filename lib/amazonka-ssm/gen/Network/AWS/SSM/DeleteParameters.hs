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
-- Module      : Network.AWS.SSM.DeleteParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a list of parameters. This API is used to delete parameters by using the Amazon EC2 console.
--
--
module Network.AWS.SSM.DeleteParameters
    (
    -- * Creating a Request
      deleteParameters
    , DeleteParameters
    -- * Request Lenses
    , dpNames

    -- * Destructuring the Response
    , deleteParametersResponse
    , DeleteParametersResponse
    -- * Response Lenses
    , drsDeletedParameters
    , drsInvalidParameters
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'deleteParameters' smart constructor.
newtype DeleteParameters = DeleteParameters'
  { _dpNames :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpNames' - The names of the parameters to delete.
deleteParameters
    :: NonEmpty Text -- ^ 'dpNames'
    -> DeleteParameters
deleteParameters pNames_ = DeleteParameters' {_dpNames = _List1 # pNames_}


-- | The names of the parameters to delete.
dpNames :: Lens' DeleteParameters (NonEmpty Text)
dpNames = lens _dpNames (\ s a -> s{_dpNames = a}) . _List1

instance AWSRequest DeleteParameters where
        type Rs DeleteParameters = DeleteParametersResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 DeleteParametersResponse' <$>
                   (x .?> "DeletedParameters") <*>
                     (x .?> "InvalidParameters")
                     <*> (pure (fromEnum s)))

instance Hashable DeleteParameters where

instance NFData DeleteParameters where

instance ToHeaders DeleteParameters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.DeleteParameters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteParameters where
        toJSON DeleteParameters'{..}
          = object (catMaybes [Just ("Names" .= _dpNames)])

instance ToPath DeleteParameters where
        toPath = const "/"

instance ToQuery DeleteParameters where
        toQuery = const mempty

-- | /See:/ 'deleteParametersResponse' smart constructor.
data DeleteParametersResponse = DeleteParametersResponse'
  { _drsDeletedParameters :: !(Maybe (List1 Text))
  , _drsInvalidParameters :: !(Maybe (List1 Text))
  , _drsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsDeletedParameters' - The names of the deleted parameters.
--
-- * 'drsInvalidParameters' - The names of parameters that weren't deleted because the parameters are not valid.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteParametersResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteParametersResponse
deleteParametersResponse pResponseStatus_ =
  DeleteParametersResponse'
    { _drsDeletedParameters = Nothing
    , _drsInvalidParameters = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | The names of the deleted parameters.
drsDeletedParameters :: Lens' DeleteParametersResponse (Maybe (NonEmpty Text))
drsDeletedParameters = lens _drsDeletedParameters (\ s a -> s{_drsDeletedParameters = a}) . mapping _List1

-- | The names of parameters that weren't deleted because the parameters are not valid.
drsInvalidParameters :: Lens' DeleteParametersResponse (Maybe (NonEmpty Text))
drsInvalidParameters = lens _drsInvalidParameters (\ s a -> s{_drsInvalidParameters = a}) . mapping _List1

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteParametersResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteParametersResponse where

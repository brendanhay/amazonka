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
-- Module      : Network.AWS.DAX.UpdateParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters of a parameter group. You can modify up to 20 parameters in a single request by submitting a list parameter name and value pairs.
--
--
module Network.AWS.DAX.UpdateParameterGroup
    (
    -- * Creating a Request
      updateParameterGroup
    , UpdateParameterGroup
    -- * Request Lenses
    , upgParameterGroupName
    , upgParameterNameValues

    -- * Destructuring the Response
    , updateParameterGroupResponse
    , UpdateParameterGroupResponse
    -- * Response Lenses
    , upgrsParameterGroup
    , upgrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateParameterGroup' smart constructor.
data UpdateParameterGroup = UpdateParameterGroup'
  { _upgParameterGroupName  :: !Text
  , _upgParameterNameValues :: ![ParameterNameValue]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upgParameterGroupName' - The name of the parameter group.
--
-- * 'upgParameterNameValues' - An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
updateParameterGroup
    :: Text -- ^ 'upgParameterGroupName'
    -> UpdateParameterGroup
updateParameterGroup pParameterGroupName_ =
  UpdateParameterGroup'
    { _upgParameterGroupName = pParameterGroupName_
    , _upgParameterNameValues = mempty
    }


-- | The name of the parameter group.
upgParameterGroupName :: Lens' UpdateParameterGroup Text
upgParameterGroupName = lens _upgParameterGroupName (\ s a -> s{_upgParameterGroupName = a})

-- | An array of name-value pairs for the parameters in the group. Each element in the array represents a single parameter.
upgParameterNameValues :: Lens' UpdateParameterGroup [ParameterNameValue]
upgParameterNameValues = lens _upgParameterNameValues (\ s a -> s{_upgParameterNameValues = a}) . _Coerce

instance AWSRequest UpdateParameterGroup where
        type Rs UpdateParameterGroup =
             UpdateParameterGroupResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 UpdateParameterGroupResponse' <$>
                   (x .?> "ParameterGroup") <*> (pure (fromEnum s)))

instance Hashable UpdateParameterGroup where

instance NFData UpdateParameterGroup where

instance ToHeaders UpdateParameterGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.UpdateParameterGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateParameterGroup where
        toJSON UpdateParameterGroup'{..}
          = object
              (catMaybes
                 [Just
                    ("ParameterGroupName" .= _upgParameterGroupName),
                  Just
                    ("ParameterNameValues" .= _upgParameterNameValues)])

instance ToPath UpdateParameterGroup where
        toPath = const "/"

instance ToQuery UpdateParameterGroup where
        toQuery = const mempty

-- | /See:/ 'updateParameterGroupResponse' smart constructor.
data UpdateParameterGroupResponse = UpdateParameterGroupResponse'
  { _upgrsParameterGroup :: !(Maybe ParameterGroup)
  , _upgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upgrsParameterGroup' - The parameter group that has been modified.
--
-- * 'upgrsResponseStatus' - -- | The response status code.
updateParameterGroupResponse
    :: Int -- ^ 'upgrsResponseStatus'
    -> UpdateParameterGroupResponse
updateParameterGroupResponse pResponseStatus_ =
  UpdateParameterGroupResponse'
    {_upgrsParameterGroup = Nothing, _upgrsResponseStatus = pResponseStatus_}


-- | The parameter group that has been modified.
upgrsParameterGroup :: Lens' UpdateParameterGroupResponse (Maybe ParameterGroup)
upgrsParameterGroup = lens _upgrsParameterGroup (\ s a -> s{_upgrsParameterGroup = a})

-- | -- | The response status code.
upgrsResponseStatus :: Lens' UpdateParameterGroupResponse Int
upgrsResponseStatus = lens _upgrsResponseStatus (\ s a -> s{_upgrsResponseStatus = a})

instance NFData UpdateParameterGroupResponse where

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
-- Module      : Network.AWS.DAX.UpdateSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing subnet group.
--
--
module Network.AWS.DAX.UpdateSubnetGroup
    (
    -- * Creating a Request
      updateSubnetGroup
    , UpdateSubnetGroup
    -- * Request Lenses
    , usgSubnetIds
    , usgDescription
    , usgSubnetGroupName

    -- * Destructuring the Response
    , updateSubnetGroupResponse
    , UpdateSubnetGroupResponse
    -- * Response Lenses
    , usgrsSubnetGroup
    , usgrsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateSubnetGroup' smart constructor.
data UpdateSubnetGroup = UpdateSubnetGroup'
  { _usgSubnetIds       :: !(Maybe [Text])
  , _usgDescription     :: !(Maybe Text)
  , _usgSubnetGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgSubnetIds' - A list of subnet IDs in the subnet group.
--
-- * 'usgDescription' - A description of the subnet group.
--
-- * 'usgSubnetGroupName' - The name of the subnet group.
updateSubnetGroup
    :: Text -- ^ 'usgSubnetGroupName'
    -> UpdateSubnetGroup
updateSubnetGroup pSubnetGroupName_ =
  UpdateSubnetGroup'
    { _usgSubnetIds = Nothing
    , _usgDescription = Nothing
    , _usgSubnetGroupName = pSubnetGroupName_
    }


-- | A list of subnet IDs in the subnet group.
usgSubnetIds :: Lens' UpdateSubnetGroup [Text]
usgSubnetIds = lens _usgSubnetIds (\ s a -> s{_usgSubnetIds = a}) . _Default . _Coerce

-- | A description of the subnet group.
usgDescription :: Lens' UpdateSubnetGroup (Maybe Text)
usgDescription = lens _usgDescription (\ s a -> s{_usgDescription = a})

-- | The name of the subnet group.
usgSubnetGroupName :: Lens' UpdateSubnetGroup Text
usgSubnetGroupName = lens _usgSubnetGroupName (\ s a -> s{_usgSubnetGroupName = a})

instance AWSRequest UpdateSubnetGroup where
        type Rs UpdateSubnetGroup = UpdateSubnetGroupResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSubnetGroupResponse' <$>
                   (x .?> "SubnetGroup") <*> (pure (fromEnum s)))

instance Hashable UpdateSubnetGroup where

instance NFData UpdateSubnetGroup where

instance ToHeaders UpdateSubnetGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.UpdateSubnetGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSubnetGroup where
        toJSON UpdateSubnetGroup'{..}
          = object
              (catMaybes
                 [("SubnetIds" .=) <$> _usgSubnetIds,
                  ("Description" .=) <$> _usgDescription,
                  Just ("SubnetGroupName" .= _usgSubnetGroupName)])

instance ToPath UpdateSubnetGroup where
        toPath = const "/"

instance ToQuery UpdateSubnetGroup where
        toQuery = const mempty

-- | /See:/ 'updateSubnetGroupResponse' smart constructor.
data UpdateSubnetGroupResponse = UpdateSubnetGroupResponse'
  { _usgrsSubnetGroup    :: !(Maybe SubnetGroup)
  , _usgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'usgrsSubnetGroup' - The subnet group that has been modified.
--
-- * 'usgrsResponseStatus' - -- | The response status code.
updateSubnetGroupResponse
    :: Int -- ^ 'usgrsResponseStatus'
    -> UpdateSubnetGroupResponse
updateSubnetGroupResponse pResponseStatus_ =
  UpdateSubnetGroupResponse'
    {_usgrsSubnetGroup = Nothing, _usgrsResponseStatus = pResponseStatus_}


-- | The subnet group that has been modified.
usgrsSubnetGroup :: Lens' UpdateSubnetGroupResponse (Maybe SubnetGroup)
usgrsSubnetGroup = lens _usgrsSubnetGroup (\ s a -> s{_usgrsSubnetGroup = a})

-- | -- | The response status code.
usgrsResponseStatus :: Lens' UpdateSubnetGroupResponse Int
usgrsResponseStatus = lens _usgrsResponseStatus (\ s a -> s{_usgrsResponseStatus = a})

instance NFData UpdateSubnetGroupResponse where

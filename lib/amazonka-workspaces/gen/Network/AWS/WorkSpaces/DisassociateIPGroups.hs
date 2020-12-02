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
-- Module      : Network.AWS.WorkSpaces.DisassociateIPGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified IP access control group from the specified directory.
--
--
module Network.AWS.WorkSpaces.DisassociateIPGroups
    (
    -- * Creating a Request
      disassociateIPGroups
    , DisassociateIPGroups
    -- * Request Lenses
    , digDirectoryId
    , digGroupIds

    -- * Destructuring the Response
    , disassociateIPGroupsResponse
    , DisassociateIPGroupsResponse
    -- * Response Lenses
    , digrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'disassociateIPGroups' smart constructor.
data DisassociateIPGroups = DisassociateIPGroups'
  { _digDirectoryId :: !Text
  , _digGroupIds    :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateIPGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digDirectoryId' - The ID of the directory.
--
-- * 'digGroupIds' - The IDs of one or more IP access control groups.
disassociateIPGroups
    :: Text -- ^ 'digDirectoryId'
    -> DisassociateIPGroups
disassociateIPGroups pDirectoryId_ =
  DisassociateIPGroups' {_digDirectoryId = pDirectoryId_, _digGroupIds = mempty}


-- | The ID of the directory.
digDirectoryId :: Lens' DisassociateIPGroups Text
digDirectoryId = lens _digDirectoryId (\ s a -> s{_digDirectoryId = a})

-- | The IDs of one or more IP access control groups.
digGroupIds :: Lens' DisassociateIPGroups [Text]
digGroupIds = lens _digGroupIds (\ s a -> s{_digGroupIds = a}) . _Coerce

instance AWSRequest DisassociateIPGroups where
        type Rs DisassociateIPGroups =
             DisassociateIPGroupsResponse
        request = postJSON workSpaces
        response
          = receiveEmpty
              (\ s h x ->
                 DisassociateIPGroupsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DisassociateIPGroups where

instance NFData DisassociateIPGroups where

instance ToHeaders DisassociateIPGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DisassociateIpGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateIPGroups where
        toJSON DisassociateIPGroups'{..}
          = object
              (catMaybes
                 [Just ("DirectoryId" .= _digDirectoryId),
                  Just ("GroupIds" .= _digGroupIds)])

instance ToPath DisassociateIPGroups where
        toPath = const "/"

instance ToQuery DisassociateIPGroups where
        toQuery = const mempty

-- | /See:/ 'disassociateIPGroupsResponse' smart constructor.
newtype DisassociateIPGroupsResponse = DisassociateIPGroupsResponse'
  { _digrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateIPGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'digrsResponseStatus' - -- | The response status code.
disassociateIPGroupsResponse
    :: Int -- ^ 'digrsResponseStatus'
    -> DisassociateIPGroupsResponse
disassociateIPGroupsResponse pResponseStatus_ =
  DisassociateIPGroupsResponse' {_digrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
digrsResponseStatus :: Lens' DisassociateIPGroupsResponse Int
digrsResponseStatus = lens _digrsResponseStatus (\ s a -> s{_digrsResponseStatus = a})

instance NFData DisassociateIPGroupsResponse where

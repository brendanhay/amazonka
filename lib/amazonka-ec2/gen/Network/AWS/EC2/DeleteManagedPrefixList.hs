{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified managed prefix list. You must first remove all references to the prefix list in your resources.
module Network.AWS.EC2.DeleteManagedPrefixList
  ( -- * Creating a Request
    deleteManagedPrefixList,
    DeleteManagedPrefixList,

    -- * Request Lenses
    dmplDryRun,
    dmplPrefixListId,

    -- * Destructuring the Response
    deleteManagedPrefixListResponse,
    DeleteManagedPrefixListResponse,

    -- * Response Lenses
    dmplrsPrefixList,
    dmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteManagedPrefixList' smart constructor.
data DeleteManagedPrefixList = DeleteManagedPrefixList'
  { _dmplDryRun ::
      !(Maybe Bool),
    _dmplPrefixListId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteManagedPrefixList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmplDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dmplPrefixListId' - The ID of the prefix list.
deleteManagedPrefixList ::
  -- | 'dmplPrefixListId'
  Text ->
  DeleteManagedPrefixList
deleteManagedPrefixList pPrefixListId_ =
  DeleteManagedPrefixList'
    { _dmplDryRun = Nothing,
      _dmplPrefixListId = pPrefixListId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dmplDryRun :: Lens' DeleteManagedPrefixList (Maybe Bool)
dmplDryRun = lens _dmplDryRun (\s a -> s {_dmplDryRun = a})

-- | The ID of the prefix list.
dmplPrefixListId :: Lens' DeleteManagedPrefixList Text
dmplPrefixListId = lens _dmplPrefixListId (\s a -> s {_dmplPrefixListId = a})

instance AWSRequest DeleteManagedPrefixList where
  type Rs DeleteManagedPrefixList = DeleteManagedPrefixListResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DeleteManagedPrefixListResponse'
            <$> (x .@? "prefixList") <*> (pure (fromEnum s))
      )

instance Hashable DeleteManagedPrefixList

instance NFData DeleteManagedPrefixList

instance ToHeaders DeleteManagedPrefixList where
  toHeaders = const mempty

instance ToPath DeleteManagedPrefixList where
  toPath = const "/"

instance ToQuery DeleteManagedPrefixList where
  toQuery DeleteManagedPrefixList' {..} =
    mconcat
      [ "Action" =: ("DeleteManagedPrefixList" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dmplDryRun,
        "PrefixListId" =: _dmplPrefixListId
      ]

-- | /See:/ 'deleteManagedPrefixListResponse' smart constructor.
data DeleteManagedPrefixListResponse = DeleteManagedPrefixListResponse'
  { _dmplrsPrefixList ::
      !(Maybe ManagedPrefixList),
    _dmplrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmplrsPrefixList' - Information about the prefix list.
--
-- * 'dmplrsResponseStatus' - -- | The response status code.
deleteManagedPrefixListResponse ::
  -- | 'dmplrsResponseStatus'
  Int ->
  DeleteManagedPrefixListResponse
deleteManagedPrefixListResponse pResponseStatus_ =
  DeleteManagedPrefixListResponse'
    { _dmplrsPrefixList = Nothing,
      _dmplrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
dmplrsPrefixList :: Lens' DeleteManagedPrefixListResponse (Maybe ManagedPrefixList)
dmplrsPrefixList = lens _dmplrsPrefixList (\s a -> s {_dmplrsPrefixList = a})

-- | -- | The response status code.
dmplrsResponseStatus :: Lens' DeleteManagedPrefixListResponse Int
dmplrsResponseStatus = lens _dmplrsResponseStatus (\s a -> s {_dmplrsResponseStatus = a})

instance NFData DeleteManagedPrefixListResponse

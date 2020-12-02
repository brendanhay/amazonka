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
-- Module      : Network.AWS.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified managed prefix list.
--
--
-- Adding or removing entries in a prefix list creates a new version of the prefix list. Changing the name of the prefix list does not affect the version.
--
-- If you specify a current version number that does not match the true current version number, the request fails.
module Network.AWS.EC2.ModifyManagedPrefixList
  ( -- * Creating a Request
    modifyManagedPrefixList,
    ModifyManagedPrefixList,

    -- * Request Lenses
    mmplCurrentVersion,
    mmplRemoveEntries,
    mmplPrefixListName,
    mmplAddEntries,
    mmplDryRun,
    mmplPrefixListId,

    -- * Destructuring the Response
    modifyManagedPrefixListResponse,
    ModifyManagedPrefixListResponse,

    -- * Response Lenses
    mmplrsPrefixList,
    mmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { _mmplCurrentVersion ::
      !(Maybe Integer),
    _mmplRemoveEntries ::
      !(Maybe [RemovePrefixListEntry]),
    _mmplPrefixListName :: !(Maybe Text),
    _mmplAddEntries ::
      !(Maybe [AddPrefixListEntry]),
    _mmplDryRun :: !(Maybe Bool),
    _mmplPrefixListId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyManagedPrefixList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmplCurrentVersion' - The current version of the prefix list.
--
-- * 'mmplRemoveEntries' - One or more entries to remove from the prefix list.
--
-- * 'mmplPrefixListName' - A name for the prefix list.
--
-- * 'mmplAddEntries' - One or more entries to add to the prefix list.
--
-- * 'mmplDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mmplPrefixListId' - The ID of the prefix list.
modifyManagedPrefixList ::
  -- | 'mmplPrefixListId'
  Text ->
  ModifyManagedPrefixList
modifyManagedPrefixList pPrefixListId_ =
  ModifyManagedPrefixList'
    { _mmplCurrentVersion = Nothing,
      _mmplRemoveEntries = Nothing,
      _mmplPrefixListName = Nothing,
      _mmplAddEntries = Nothing,
      _mmplDryRun = Nothing,
      _mmplPrefixListId = pPrefixListId_
    }

-- | The current version of the prefix list.
mmplCurrentVersion :: Lens' ModifyManagedPrefixList (Maybe Integer)
mmplCurrentVersion = lens _mmplCurrentVersion (\s a -> s {_mmplCurrentVersion = a})

-- | One or more entries to remove from the prefix list.
mmplRemoveEntries :: Lens' ModifyManagedPrefixList [RemovePrefixListEntry]
mmplRemoveEntries = lens _mmplRemoveEntries (\s a -> s {_mmplRemoveEntries = a}) . _Default . _Coerce

-- | A name for the prefix list.
mmplPrefixListName :: Lens' ModifyManagedPrefixList (Maybe Text)
mmplPrefixListName = lens _mmplPrefixListName (\s a -> s {_mmplPrefixListName = a})

-- | One or more entries to add to the prefix list.
mmplAddEntries :: Lens' ModifyManagedPrefixList [AddPrefixListEntry]
mmplAddEntries = lens _mmplAddEntries (\s a -> s {_mmplAddEntries = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mmplDryRun :: Lens' ModifyManagedPrefixList (Maybe Bool)
mmplDryRun = lens _mmplDryRun (\s a -> s {_mmplDryRun = a})

-- | The ID of the prefix list.
mmplPrefixListId :: Lens' ModifyManagedPrefixList Text
mmplPrefixListId = lens _mmplPrefixListId (\s a -> s {_mmplPrefixListId = a})

instance AWSRequest ModifyManagedPrefixList where
  type Rs ModifyManagedPrefixList = ModifyManagedPrefixListResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            <$> (x .@? "prefixList") <*> (pure (fromEnum s))
      )

instance Hashable ModifyManagedPrefixList

instance NFData ModifyManagedPrefixList

instance ToHeaders ModifyManagedPrefixList where
  toHeaders = const mempty

instance ToPath ModifyManagedPrefixList where
  toPath = const "/"

instance ToQuery ModifyManagedPrefixList where
  toQuery ModifyManagedPrefixList' {..} =
    mconcat
      [ "Action" =: ("ModifyManagedPrefixList" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "CurrentVersion" =: _mmplCurrentVersion,
        toQuery (toQueryList "RemoveEntry" <$> _mmplRemoveEntries),
        "PrefixListName" =: _mmplPrefixListName,
        toQuery (toQueryList "AddEntry" <$> _mmplAddEntries),
        "DryRun" =: _mmplDryRun,
        "PrefixListId" =: _mmplPrefixListId
      ]

-- | /See:/ 'modifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { _mmplrsPrefixList ::
      !(Maybe ManagedPrefixList),
    _mmplrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ModifyManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmplrsPrefixList' - Information about the prefix list.
--
-- * 'mmplrsResponseStatus' - -- | The response status code.
modifyManagedPrefixListResponse ::
  -- | 'mmplrsResponseStatus'
  Int ->
  ModifyManagedPrefixListResponse
modifyManagedPrefixListResponse pResponseStatus_ =
  ModifyManagedPrefixListResponse'
    { _mmplrsPrefixList = Nothing,
      _mmplrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
mmplrsPrefixList :: Lens' ModifyManagedPrefixListResponse (Maybe ManagedPrefixList)
mmplrsPrefixList = lens _mmplrsPrefixList (\s a -> s {_mmplrsPrefixList = a})

-- | -- | The response status code.
mmplrsResponseStatus :: Lens' ModifyManagedPrefixListResponse Int
mmplrsResponseStatus = lens _mmplrsResponseStatus (\s a -> s {_mmplrsResponseStatus = a})

instance NFData ModifyManagedPrefixListResponse

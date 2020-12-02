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
-- Module      : Network.AWS.EC2.CreateManagedPrefixList
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed prefix list. You can specify one or more entries for the prefix list. Each entry consists of a CIDR block and an optional description.
--
--
-- You must specify the maximum number of entries for the prefix list. The maximum number of entries cannot be changed later.
module Network.AWS.EC2.CreateManagedPrefixList
  ( -- * Creating a Request
    createManagedPrefixList,
    CreateManagedPrefixList,

    -- * Request Lenses
    cmplClientToken,
    cmplEntries,
    cmplTagSpecifications,
    cmplDryRun,
    cmplPrefixListName,
    cmplMaxEntries,
    cmplAddressFamily,

    -- * Destructuring the Response
    createManagedPrefixListResponse,
    CreateManagedPrefixListResponse,

    -- * Response Lenses
    cmplrsPrefixList,
    cmplrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createManagedPrefixList' smart constructor.
data CreateManagedPrefixList = CreateManagedPrefixList'
  { _cmplClientToken ::
      !(Maybe Text),
    _cmplEntries ::
      !(Maybe [AddPrefixListEntry]),
    _cmplTagSpecifications ::
      !(Maybe [TagSpecification]),
    _cmplDryRun :: !(Maybe Bool),
    _cmplPrefixListName :: !Text,
    _cmplMaxEntries :: !Int,
    _cmplAddressFamily :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateManagedPrefixList' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmplClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Up to 255 UTF-8 characters in length.
--
-- * 'cmplEntries' - One or more entries for the prefix list.
--
-- * 'cmplTagSpecifications' - The tags to apply to the prefix list during creation.
--
-- * 'cmplDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cmplPrefixListName' - A name for the prefix list. Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
--
-- * 'cmplMaxEntries' - The maximum number of entries for the prefix list.
--
-- * 'cmplAddressFamily' - The IP address type. Valid Values: @IPv4@ | @IPv6@
createManagedPrefixList ::
  -- | 'cmplPrefixListName'
  Text ->
  -- | 'cmplMaxEntries'
  Int ->
  -- | 'cmplAddressFamily'
  Text ->
  CreateManagedPrefixList
createManagedPrefixList
  pPrefixListName_
  pMaxEntries_
  pAddressFamily_ =
    CreateManagedPrefixList'
      { _cmplClientToken = Nothing,
        _cmplEntries = Nothing,
        _cmplTagSpecifications = Nothing,
        _cmplDryRun = Nothing,
        _cmplPrefixListName = pPrefixListName_,
        _cmplMaxEntries = pMaxEntries_,
        _cmplAddressFamily = pAddressFamily_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> . Constraints: Up to 255 UTF-8 characters in length.
cmplClientToken :: Lens' CreateManagedPrefixList (Maybe Text)
cmplClientToken = lens _cmplClientToken (\s a -> s {_cmplClientToken = a})

-- | One or more entries for the prefix list.
cmplEntries :: Lens' CreateManagedPrefixList [AddPrefixListEntry]
cmplEntries = lens _cmplEntries (\s a -> s {_cmplEntries = a}) . _Default . _Coerce

-- | The tags to apply to the prefix list during creation.
cmplTagSpecifications :: Lens' CreateManagedPrefixList [TagSpecification]
cmplTagSpecifications = lens _cmplTagSpecifications (\s a -> s {_cmplTagSpecifications = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cmplDryRun :: Lens' CreateManagedPrefixList (Maybe Bool)
cmplDryRun = lens _cmplDryRun (\s a -> s {_cmplDryRun = a})

-- | A name for the prefix list. Constraints: Up to 255 characters in length. The name cannot start with @com.amazonaws@ .
cmplPrefixListName :: Lens' CreateManagedPrefixList Text
cmplPrefixListName = lens _cmplPrefixListName (\s a -> s {_cmplPrefixListName = a})

-- | The maximum number of entries for the prefix list.
cmplMaxEntries :: Lens' CreateManagedPrefixList Int
cmplMaxEntries = lens _cmplMaxEntries (\s a -> s {_cmplMaxEntries = a})

-- | The IP address type. Valid Values: @IPv4@ | @IPv6@
cmplAddressFamily :: Lens' CreateManagedPrefixList Text
cmplAddressFamily = lens _cmplAddressFamily (\s a -> s {_cmplAddressFamily = a})

instance AWSRequest CreateManagedPrefixList where
  type Rs CreateManagedPrefixList = CreateManagedPrefixListResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          CreateManagedPrefixListResponse'
            <$> (x .@? "prefixList") <*> (pure (fromEnum s))
      )

instance Hashable CreateManagedPrefixList

instance NFData CreateManagedPrefixList

instance ToHeaders CreateManagedPrefixList where
  toHeaders = const mempty

instance ToPath CreateManagedPrefixList where
  toPath = const "/"

instance ToQuery CreateManagedPrefixList where
  toQuery CreateManagedPrefixList' {..} =
    mconcat
      [ "Action" =: ("CreateManagedPrefixList" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _cmplClientToken,
        toQuery (toQueryList "Entry" <$> _cmplEntries),
        toQuery
          (toQueryList "TagSpecification" <$> _cmplTagSpecifications),
        "DryRun" =: _cmplDryRun,
        "PrefixListName" =: _cmplPrefixListName,
        "MaxEntries" =: _cmplMaxEntries,
        "AddressFamily" =: _cmplAddressFamily
      ]

-- | /See:/ 'createManagedPrefixListResponse' smart constructor.
data CreateManagedPrefixListResponse = CreateManagedPrefixListResponse'
  { _cmplrsPrefixList ::
      !(Maybe ManagedPrefixList),
    _cmplrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateManagedPrefixListResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmplrsPrefixList' - Information about the prefix list.
--
-- * 'cmplrsResponseStatus' - -- | The response status code.
createManagedPrefixListResponse ::
  -- | 'cmplrsResponseStatus'
  Int ->
  CreateManagedPrefixListResponse
createManagedPrefixListResponse pResponseStatus_ =
  CreateManagedPrefixListResponse'
    { _cmplrsPrefixList = Nothing,
      _cmplrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list.
cmplrsPrefixList :: Lens' CreateManagedPrefixListResponse (Maybe ManagedPrefixList)
cmplrsPrefixList = lens _cmplrsPrefixList (\s a -> s {_cmplrsPrefixList = a})

-- | -- | The response status code.
cmplrsResponseStatus :: Lens' CreateManagedPrefixListResponse Int
cmplrsResponseStatus = lens _cmplrsResponseStatus (\s a -> s {_cmplrsResponseStatus = a})

instance NFData CreateManagedPrefixListResponse

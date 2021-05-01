{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a managed prefix list. You can specify one or more entries for
-- the prefix list. Each entry consists of a CIDR block and an optional
-- description.
--
-- You must specify the maximum number of entries for the prefix list. The
-- maximum number of entries cannot be changed later.
module Network.AWS.EC2.CreateManagedPrefixList
  ( -- * Creating a Request
    CreateManagedPrefixList (..),
    newCreateManagedPrefixList,

    -- * Request Lenses
    createManagedPrefixList_tagSpecifications,
    createManagedPrefixList_dryRun,
    createManagedPrefixList_clientToken,
    createManagedPrefixList_entries,
    createManagedPrefixList_prefixListName,
    createManagedPrefixList_maxEntries,
    createManagedPrefixList_addressFamily,

    -- * Destructuring the Response
    CreateManagedPrefixListResponse (..),
    newCreateManagedPrefixListResponse,

    -- * Response Lenses
    createManagedPrefixListResponse_prefixList,
    createManagedPrefixListResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateManagedPrefixList' smart constructor.
data CreateManagedPrefixList = CreateManagedPrefixList'
  { -- | The tags to apply to the prefix list during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraints: Up to 255 UTF-8 characters in length.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | One or more entries for the prefix list.
    entries :: Prelude.Maybe [AddPrefixListEntry],
    -- | A name for the prefix list.
    --
    -- Constraints: Up to 255 characters in length. The name cannot start with
    -- @com.amazonaws@.
    prefixListName :: Prelude.Text,
    -- | The maximum number of entries for the prefix list.
    maxEntries :: Prelude.Int,
    -- | The IP address type.
    --
    -- Valid Values: @IPv4@ | @IPv6@
    addressFamily :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createManagedPrefixList_tagSpecifications' - The tags to apply to the prefix list during creation.
--
-- 'dryRun', 'createManagedPrefixList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'clientToken', 'createManagedPrefixList_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Up to 255 UTF-8 characters in length.
--
-- 'entries', 'createManagedPrefixList_entries' - One or more entries for the prefix list.
--
-- 'prefixListName', 'createManagedPrefixList_prefixListName' - A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with
-- @com.amazonaws@.
--
-- 'maxEntries', 'createManagedPrefixList_maxEntries' - The maximum number of entries for the prefix list.
--
-- 'addressFamily', 'createManagedPrefixList_addressFamily' - The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@
newCreateManagedPrefixList ::
  -- | 'prefixListName'
  Prelude.Text ->
  -- | 'maxEntries'
  Prelude.Int ->
  -- | 'addressFamily'
  Prelude.Text ->
  CreateManagedPrefixList
newCreateManagedPrefixList
  pPrefixListName_
  pMaxEntries_
  pAddressFamily_ =
    CreateManagedPrefixList'
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        entries = Prelude.Nothing,
        prefixListName = pPrefixListName_,
        maxEntries = pMaxEntries_,
        addressFamily = pAddressFamily_
      }

-- | The tags to apply to the prefix list during creation.
createManagedPrefixList_tagSpecifications :: Lens.Lens' CreateManagedPrefixList (Prelude.Maybe [TagSpecification])
createManagedPrefixList_tagSpecifications = Lens.lens (\CreateManagedPrefixList' {tagSpecifications} -> tagSpecifications) (\s@CreateManagedPrefixList' {} a -> s {tagSpecifications = a} :: CreateManagedPrefixList) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createManagedPrefixList_dryRun :: Lens.Lens' CreateManagedPrefixList (Prelude.Maybe Prelude.Bool)
createManagedPrefixList_dryRun = Lens.lens (\CreateManagedPrefixList' {dryRun} -> dryRun) (\s@CreateManagedPrefixList' {} a -> s {dryRun = a} :: CreateManagedPrefixList)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraints: Up to 255 UTF-8 characters in length.
createManagedPrefixList_clientToken :: Lens.Lens' CreateManagedPrefixList (Prelude.Maybe Prelude.Text)
createManagedPrefixList_clientToken = Lens.lens (\CreateManagedPrefixList' {clientToken} -> clientToken) (\s@CreateManagedPrefixList' {} a -> s {clientToken = a} :: CreateManagedPrefixList)

-- | One or more entries for the prefix list.
createManagedPrefixList_entries :: Lens.Lens' CreateManagedPrefixList (Prelude.Maybe [AddPrefixListEntry])
createManagedPrefixList_entries = Lens.lens (\CreateManagedPrefixList' {entries} -> entries) (\s@CreateManagedPrefixList' {} a -> s {entries = a} :: CreateManagedPrefixList) Prelude.. Lens.mapping Prelude._Coerce

-- | A name for the prefix list.
--
-- Constraints: Up to 255 characters in length. The name cannot start with
-- @com.amazonaws@.
createManagedPrefixList_prefixListName :: Lens.Lens' CreateManagedPrefixList Prelude.Text
createManagedPrefixList_prefixListName = Lens.lens (\CreateManagedPrefixList' {prefixListName} -> prefixListName) (\s@CreateManagedPrefixList' {} a -> s {prefixListName = a} :: CreateManagedPrefixList)

-- | The maximum number of entries for the prefix list.
createManagedPrefixList_maxEntries :: Lens.Lens' CreateManagedPrefixList Prelude.Int
createManagedPrefixList_maxEntries = Lens.lens (\CreateManagedPrefixList' {maxEntries} -> maxEntries) (\s@CreateManagedPrefixList' {} a -> s {maxEntries = a} :: CreateManagedPrefixList)

-- | The IP address type.
--
-- Valid Values: @IPv4@ | @IPv6@
createManagedPrefixList_addressFamily :: Lens.Lens' CreateManagedPrefixList Prelude.Text
createManagedPrefixList_addressFamily = Lens.lens (\CreateManagedPrefixList' {addressFamily} -> addressFamily) (\s@CreateManagedPrefixList' {} a -> s {addressFamily = a} :: CreateManagedPrefixList)

instance Prelude.AWSRequest CreateManagedPrefixList where
  type
    Rs CreateManagedPrefixList =
      CreateManagedPrefixListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateManagedPrefixListResponse'
            Prelude.<$> (x Prelude..@? "prefixList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateManagedPrefixList

instance Prelude.NFData CreateManagedPrefixList

instance Prelude.ToHeaders CreateManagedPrefixList where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateManagedPrefixList where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateManagedPrefixList where
  toQuery CreateManagedPrefixList' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateManagedPrefixList" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "ClientToken" Prelude.=: clientToken,
        Prelude.toQuery
          (Prelude.toQueryList "Entry" Prelude.<$> entries),
        "PrefixListName" Prelude.=: prefixListName,
        "MaxEntries" Prelude.=: maxEntries,
        "AddressFamily" Prelude.=: addressFamily
      ]

-- | /See:/ 'newCreateManagedPrefixListResponse' smart constructor.
data CreateManagedPrefixListResponse = CreateManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Prelude.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateManagedPrefixListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixList', 'createManagedPrefixListResponse_prefixList' - Information about the prefix list.
--
-- 'httpStatus', 'createManagedPrefixListResponse_httpStatus' - The response's http status code.
newCreateManagedPrefixListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateManagedPrefixListResponse
newCreateManagedPrefixListResponse pHttpStatus_ =
  CreateManagedPrefixListResponse'
    { prefixList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
createManagedPrefixListResponse_prefixList :: Lens.Lens' CreateManagedPrefixListResponse (Prelude.Maybe ManagedPrefixList)
createManagedPrefixListResponse_prefixList = Lens.lens (\CreateManagedPrefixListResponse' {prefixList} -> prefixList) (\s@CreateManagedPrefixListResponse' {} a -> s {prefixList = a} :: CreateManagedPrefixListResponse)

-- | The response's http status code.
createManagedPrefixListResponse_httpStatus :: Lens.Lens' CreateManagedPrefixListResponse Prelude.Int
createManagedPrefixListResponse_httpStatus = Lens.lens (\CreateManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@CreateManagedPrefixListResponse' {} a -> s {httpStatus = a} :: CreateManagedPrefixListResponse)

instance
  Prelude.NFData
    CreateManagedPrefixListResponse

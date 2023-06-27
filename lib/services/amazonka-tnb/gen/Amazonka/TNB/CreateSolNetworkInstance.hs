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
-- Module      : Amazonka.TNB.CreateSolNetworkInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a network instance.
--
-- A network instance is a single network created in Amazon Web Services
-- TNB that can be deployed and on which life-cycle operations (like
-- terminate, update, and delete) can be performed. Creating a network
-- instance is the third step after creating a network package. For more
-- information about network instances,
-- <https://docs.aws.amazon.com/tnb/latest/ug/network-instances.html Network instances>
-- in the /Amazon Web Services Telco Network Builder User Guide/.
--
-- Once you create a network instance, you can instantiate it. To
-- instantiate a network, see
-- <https://docs.aws.amazon.com/tnb/latest/APIReference/API_InstantiateSolNetworkInstance.html InstantiateSolNetworkInstance>.
module Amazonka.TNB.CreateSolNetworkInstance
  ( -- * Creating a Request
    CreateSolNetworkInstance (..),
    newCreateSolNetworkInstance,

    -- * Request Lenses
    createSolNetworkInstance_nsDescription,
    createSolNetworkInstance_tags,
    createSolNetworkInstance_nsName,
    createSolNetworkInstance_nsdInfoId,

    -- * Destructuring the Response
    CreateSolNetworkInstanceResponse (..),
    newCreateSolNetworkInstanceResponse,

    -- * Response Lenses
    createSolNetworkInstanceResponse_tags,
    createSolNetworkInstanceResponse_httpStatus,
    createSolNetworkInstanceResponse_arn,
    createSolNetworkInstanceResponse_id,
    createSolNetworkInstanceResponse_nsInstanceName,
    createSolNetworkInstanceResponse_nsdInfoId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TNB.Types

-- | /See:/ 'newCreateSolNetworkInstance' smart constructor.
data CreateSolNetworkInstance = CreateSolNetworkInstance'
  { -- | Network instance description.
    nsDescription :: Prelude.Maybe Prelude.Text,
    -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Network instance name.
    nsName :: Prelude.Text,
    -- | ID for network service descriptor.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolNetworkInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nsDescription', 'createSolNetworkInstance_nsDescription' - Network instance description.
--
-- 'tags', 'createSolNetworkInstance_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'nsName', 'createSolNetworkInstance_nsName' - Network instance name.
--
-- 'nsdInfoId', 'createSolNetworkInstance_nsdInfoId' - ID for network service descriptor.
newCreateSolNetworkInstance ::
  -- | 'nsName'
  Prelude.Text ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  CreateSolNetworkInstance
newCreateSolNetworkInstance pNsName_ pNsdInfoId_ =
  CreateSolNetworkInstance'
    { nsDescription =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      nsName = pNsName_,
      nsdInfoId = pNsdInfoId_
    }

-- | Network instance description.
createSolNetworkInstance_nsDescription :: Lens.Lens' CreateSolNetworkInstance (Prelude.Maybe Prelude.Text)
createSolNetworkInstance_nsDescription = Lens.lens (\CreateSolNetworkInstance' {nsDescription} -> nsDescription) (\s@CreateSolNetworkInstance' {} a -> s {nsDescription = a} :: CreateSolNetworkInstance)

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolNetworkInstance_tags :: Lens.Lens' CreateSolNetworkInstance (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolNetworkInstance_tags = Lens.lens (\CreateSolNetworkInstance' {tags} -> tags) (\s@CreateSolNetworkInstance' {} a -> s {tags = a} :: CreateSolNetworkInstance) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Network instance name.
createSolNetworkInstance_nsName :: Lens.Lens' CreateSolNetworkInstance Prelude.Text
createSolNetworkInstance_nsName = Lens.lens (\CreateSolNetworkInstance' {nsName} -> nsName) (\s@CreateSolNetworkInstance' {} a -> s {nsName = a} :: CreateSolNetworkInstance)

-- | ID for network service descriptor.
createSolNetworkInstance_nsdInfoId :: Lens.Lens' CreateSolNetworkInstance Prelude.Text
createSolNetworkInstance_nsdInfoId = Lens.lens (\CreateSolNetworkInstance' {nsdInfoId} -> nsdInfoId) (\s@CreateSolNetworkInstance' {} a -> s {nsdInfoId = a} :: CreateSolNetworkInstance)

instance Core.AWSRequest CreateSolNetworkInstance where
  type
    AWSResponse CreateSolNetworkInstance =
      CreateSolNetworkInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSolNetworkInstanceResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "nsInstanceName")
            Prelude.<*> (x Data..:> "nsdInfoId")
      )

instance Prelude.Hashable CreateSolNetworkInstance where
  hashWithSalt _salt CreateSolNetworkInstance' {..} =
    _salt
      `Prelude.hashWithSalt` nsDescription
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` nsName
      `Prelude.hashWithSalt` nsdInfoId

instance Prelude.NFData CreateSolNetworkInstance where
  rnf CreateSolNetworkInstance' {..} =
    Prelude.rnf nsDescription
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf nsName
      `Prelude.seq` Prelude.rnf nsdInfoId

instance Data.ToHeaders CreateSolNetworkInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSolNetworkInstance where
  toJSON CreateSolNetworkInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nsDescription" Data..=) Prelude.<$> nsDescription,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("nsName" Data..= nsName),
            Prelude.Just ("nsdInfoId" Data..= nsdInfoId)
          ]
      )

instance Data.ToPath CreateSolNetworkInstance where
  toPath = Prelude.const "/sol/nslcm/v1/ns_instances"

instance Data.ToQuery CreateSolNetworkInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSolNetworkInstanceResponse' smart constructor.
data CreateSolNetworkInstanceResponse = CreateSolNetworkInstanceResponse'
  { -- | A tag is a label that you assign to an Amazon Web Services resource.
    -- Each tag consists of a key and an optional value. You can use tags to
    -- search and filter your resources or track your Amazon Web Services
    -- costs.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Network instance ARN.
    arn :: Prelude.Text,
    -- | Network instance ID.
    id :: Prelude.Text,
    -- | Network instance name.
    nsInstanceName :: Prelude.Text,
    -- | Network service descriptor ID.
    nsdInfoId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSolNetworkInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSolNetworkInstanceResponse_tags' - A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
--
-- 'httpStatus', 'createSolNetworkInstanceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createSolNetworkInstanceResponse_arn' - Network instance ARN.
--
-- 'id', 'createSolNetworkInstanceResponse_id' - Network instance ID.
--
-- 'nsInstanceName', 'createSolNetworkInstanceResponse_nsInstanceName' - Network instance name.
--
-- 'nsdInfoId', 'createSolNetworkInstanceResponse_nsdInfoId' - Network service descriptor ID.
newCreateSolNetworkInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'nsInstanceName'
  Prelude.Text ->
  -- | 'nsdInfoId'
  Prelude.Text ->
  CreateSolNetworkInstanceResponse
newCreateSolNetworkInstanceResponse
  pHttpStatus_
  pArn_
  pId_
  pNsInstanceName_
  pNsdInfoId_ =
    CreateSolNetworkInstanceResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        nsInstanceName = pNsInstanceName_,
        nsdInfoId = pNsdInfoId_
      }

-- | A tag is a label that you assign to an Amazon Web Services resource.
-- Each tag consists of a key and an optional value. You can use tags to
-- search and filter your resources or track your Amazon Web Services
-- costs.
createSolNetworkInstanceResponse_tags :: Lens.Lens' CreateSolNetworkInstanceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSolNetworkInstanceResponse_tags = Lens.lens (\CreateSolNetworkInstanceResponse' {tags} -> tags) (\s@CreateSolNetworkInstanceResponse' {} a -> s {tags = a} :: CreateSolNetworkInstanceResponse) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The response's http status code.
createSolNetworkInstanceResponse_httpStatus :: Lens.Lens' CreateSolNetworkInstanceResponse Prelude.Int
createSolNetworkInstanceResponse_httpStatus = Lens.lens (\CreateSolNetworkInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateSolNetworkInstanceResponse' {} a -> s {httpStatus = a} :: CreateSolNetworkInstanceResponse)

-- | Network instance ARN.
createSolNetworkInstanceResponse_arn :: Lens.Lens' CreateSolNetworkInstanceResponse Prelude.Text
createSolNetworkInstanceResponse_arn = Lens.lens (\CreateSolNetworkInstanceResponse' {arn} -> arn) (\s@CreateSolNetworkInstanceResponse' {} a -> s {arn = a} :: CreateSolNetworkInstanceResponse)

-- | Network instance ID.
createSolNetworkInstanceResponse_id :: Lens.Lens' CreateSolNetworkInstanceResponse Prelude.Text
createSolNetworkInstanceResponse_id = Lens.lens (\CreateSolNetworkInstanceResponse' {id} -> id) (\s@CreateSolNetworkInstanceResponse' {} a -> s {id = a} :: CreateSolNetworkInstanceResponse)

-- | Network instance name.
createSolNetworkInstanceResponse_nsInstanceName :: Lens.Lens' CreateSolNetworkInstanceResponse Prelude.Text
createSolNetworkInstanceResponse_nsInstanceName = Lens.lens (\CreateSolNetworkInstanceResponse' {nsInstanceName} -> nsInstanceName) (\s@CreateSolNetworkInstanceResponse' {} a -> s {nsInstanceName = a} :: CreateSolNetworkInstanceResponse)

-- | Network service descriptor ID.
createSolNetworkInstanceResponse_nsdInfoId :: Lens.Lens' CreateSolNetworkInstanceResponse Prelude.Text
createSolNetworkInstanceResponse_nsdInfoId = Lens.lens (\CreateSolNetworkInstanceResponse' {nsdInfoId} -> nsdInfoId) (\s@CreateSolNetworkInstanceResponse' {} a -> s {nsdInfoId = a} :: CreateSolNetworkInstanceResponse)

instance
  Prelude.NFData
    CreateSolNetworkInstanceResponse
  where
  rnf CreateSolNetworkInstanceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf nsInstanceName
      `Prelude.seq` Prelude.rnf nsdInfoId

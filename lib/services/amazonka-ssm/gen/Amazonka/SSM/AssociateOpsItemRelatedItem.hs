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
-- Module      : Amazonka.SSM.AssociateOpsItemRelatedItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a related item to a Systems Manager OpsCenter OpsItem. For
-- example, you can associate an Incident Manager incident or analysis with
-- an OpsItem. Incident Manager and OpsCenter are capabilities of Amazon
-- Web Services Systems Manager.
module Amazonka.SSM.AssociateOpsItemRelatedItem
  ( -- * Creating a Request
    AssociateOpsItemRelatedItem (..),
    newAssociateOpsItemRelatedItem,

    -- * Request Lenses
    associateOpsItemRelatedItem_opsItemId,
    associateOpsItemRelatedItem_associationType,
    associateOpsItemRelatedItem_resourceType,
    associateOpsItemRelatedItem_resourceUri,

    -- * Destructuring the Response
    AssociateOpsItemRelatedItemResponse (..),
    newAssociateOpsItemRelatedItemResponse,

    -- * Response Lenses
    associateOpsItemRelatedItemResponse_associationId,
    associateOpsItemRelatedItemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newAssociateOpsItemRelatedItem' smart constructor.
data AssociateOpsItemRelatedItem = AssociateOpsItemRelatedItem'
  { -- | The ID of the OpsItem to which you want to associate a resource as a
    -- related item.
    opsItemId :: Prelude.Text,
    -- | The type of association that you want to create between an OpsItem and a
    -- resource. OpsCenter supports @IsParentOf@ and @RelatesTo@ association
    -- types.
    associationType :: Prelude.Text,
    -- | The type of resource that you want to associate with an OpsItem.
    -- OpsCenter supports the following types:
    --
    -- @AWS::SSMIncidents::IncidentRecord@: an Incident Manager incident.
    --
    -- @AWS::SSM::Document@: a Systems Manager (SSM) document.
    resourceType :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services resource that
    -- you want to associate with the OpsItem.
    resourceUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateOpsItemRelatedItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'opsItemId', 'associateOpsItemRelatedItem_opsItemId' - The ID of the OpsItem to which you want to associate a resource as a
-- related item.
--
-- 'associationType', 'associateOpsItemRelatedItem_associationType' - The type of association that you want to create between an OpsItem and a
-- resource. OpsCenter supports @IsParentOf@ and @RelatesTo@ association
-- types.
--
-- 'resourceType', 'associateOpsItemRelatedItem_resourceType' - The type of resource that you want to associate with an OpsItem.
-- OpsCenter supports the following types:
--
-- @AWS::SSMIncidents::IncidentRecord@: an Incident Manager incident.
--
-- @AWS::SSM::Document@: a Systems Manager (SSM) document.
--
-- 'resourceUri', 'associateOpsItemRelatedItem_resourceUri' - The Amazon Resource Name (ARN) of the Amazon Web Services resource that
-- you want to associate with the OpsItem.
newAssociateOpsItemRelatedItem ::
  -- | 'opsItemId'
  Prelude.Text ->
  -- | 'associationType'
  Prelude.Text ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'resourceUri'
  Prelude.Text ->
  AssociateOpsItemRelatedItem
newAssociateOpsItemRelatedItem
  pOpsItemId_
  pAssociationType_
  pResourceType_
  pResourceUri_ =
    AssociateOpsItemRelatedItem'
      { opsItemId =
          pOpsItemId_,
        associationType = pAssociationType_,
        resourceType = pResourceType_,
        resourceUri = pResourceUri_
      }

-- | The ID of the OpsItem to which you want to associate a resource as a
-- related item.
associateOpsItemRelatedItem_opsItemId :: Lens.Lens' AssociateOpsItemRelatedItem Prelude.Text
associateOpsItemRelatedItem_opsItemId = Lens.lens (\AssociateOpsItemRelatedItem' {opsItemId} -> opsItemId) (\s@AssociateOpsItemRelatedItem' {} a -> s {opsItemId = a} :: AssociateOpsItemRelatedItem)

-- | The type of association that you want to create between an OpsItem and a
-- resource. OpsCenter supports @IsParentOf@ and @RelatesTo@ association
-- types.
associateOpsItemRelatedItem_associationType :: Lens.Lens' AssociateOpsItemRelatedItem Prelude.Text
associateOpsItemRelatedItem_associationType = Lens.lens (\AssociateOpsItemRelatedItem' {associationType} -> associationType) (\s@AssociateOpsItemRelatedItem' {} a -> s {associationType = a} :: AssociateOpsItemRelatedItem)

-- | The type of resource that you want to associate with an OpsItem.
-- OpsCenter supports the following types:
--
-- @AWS::SSMIncidents::IncidentRecord@: an Incident Manager incident.
--
-- @AWS::SSM::Document@: a Systems Manager (SSM) document.
associateOpsItemRelatedItem_resourceType :: Lens.Lens' AssociateOpsItemRelatedItem Prelude.Text
associateOpsItemRelatedItem_resourceType = Lens.lens (\AssociateOpsItemRelatedItem' {resourceType} -> resourceType) (\s@AssociateOpsItemRelatedItem' {} a -> s {resourceType = a} :: AssociateOpsItemRelatedItem)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services resource that
-- you want to associate with the OpsItem.
associateOpsItemRelatedItem_resourceUri :: Lens.Lens' AssociateOpsItemRelatedItem Prelude.Text
associateOpsItemRelatedItem_resourceUri = Lens.lens (\AssociateOpsItemRelatedItem' {resourceUri} -> resourceUri) (\s@AssociateOpsItemRelatedItem' {} a -> s {resourceUri = a} :: AssociateOpsItemRelatedItem)

instance Core.AWSRequest AssociateOpsItemRelatedItem where
  type
    AWSResponse AssociateOpsItemRelatedItem =
      AssociateOpsItemRelatedItemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateOpsItemRelatedItemResponse'
            Prelude.<$> (x Core..?> "AssociationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateOpsItemRelatedItem where
  hashWithSalt _salt AssociateOpsItemRelatedItem' {..} =
    _salt `Prelude.hashWithSalt` opsItemId
      `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceUri

instance Prelude.NFData AssociateOpsItemRelatedItem where
  rnf AssociateOpsItemRelatedItem' {..} =
    Prelude.rnf opsItemId
      `Prelude.seq` Prelude.rnf associationType
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceUri

instance Core.ToHeaders AssociateOpsItemRelatedItem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.AssociateOpsItemRelatedItem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateOpsItemRelatedItem where
  toJSON AssociateOpsItemRelatedItem' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("OpsItemId" Core..= opsItemId),
            Prelude.Just
              ("AssociationType" Core..= associationType),
            Prelude.Just ("ResourceType" Core..= resourceType),
            Prelude.Just ("ResourceUri" Core..= resourceUri)
          ]
      )

instance Core.ToPath AssociateOpsItemRelatedItem where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateOpsItemRelatedItem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateOpsItemRelatedItemResponse' smart constructor.
data AssociateOpsItemRelatedItemResponse = AssociateOpsItemRelatedItemResponse'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateOpsItemRelatedItemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'associateOpsItemRelatedItemResponse_associationId' - The association ID.
--
-- 'httpStatus', 'associateOpsItemRelatedItemResponse_httpStatus' - The response's http status code.
newAssociateOpsItemRelatedItemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateOpsItemRelatedItemResponse
newAssociateOpsItemRelatedItemResponse pHttpStatus_ =
  AssociateOpsItemRelatedItemResponse'
    { associationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The association ID.
associateOpsItemRelatedItemResponse_associationId :: Lens.Lens' AssociateOpsItemRelatedItemResponse (Prelude.Maybe Prelude.Text)
associateOpsItemRelatedItemResponse_associationId = Lens.lens (\AssociateOpsItemRelatedItemResponse' {associationId} -> associationId) (\s@AssociateOpsItemRelatedItemResponse' {} a -> s {associationId = a} :: AssociateOpsItemRelatedItemResponse)

-- | The response's http status code.
associateOpsItemRelatedItemResponse_httpStatus :: Lens.Lens' AssociateOpsItemRelatedItemResponse Prelude.Int
associateOpsItemRelatedItemResponse_httpStatus = Lens.lens (\AssociateOpsItemRelatedItemResponse' {httpStatus} -> httpStatus) (\s@AssociateOpsItemRelatedItemResponse' {} a -> s {httpStatus = a} :: AssociateOpsItemRelatedItemResponse)

instance
  Prelude.NFData
    AssociateOpsItemRelatedItemResponse
  where
  rnf AssociateOpsItemRelatedItemResponse' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf httpStatus

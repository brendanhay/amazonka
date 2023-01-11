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
-- Module      : Amazonka.SageMaker.AddAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an /association/ between the source and the destination. A
-- source can be associated with multiple destinations, and a destination
-- can be associated with multiple sources. An association is a lineage
-- tracking entity. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/lineage-tracking.html Amazon SageMaker ML Lineage Tracking>.
module Amazonka.SageMaker.AddAssociation
  ( -- * Creating a Request
    AddAssociation (..),
    newAddAssociation,

    -- * Request Lenses
    addAssociation_associationType,
    addAssociation_sourceArn,
    addAssociation_destinationArn,

    -- * Destructuring the Response
    AddAssociationResponse (..),
    newAddAssociationResponse,

    -- * Response Lenses
    addAssociationResponse_destinationArn,
    addAssociationResponse_sourceArn,
    addAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newAddAssociation' smart constructor.
data AddAssociation = AddAssociation'
  { -- | The type of association. The following are suggested uses for each type.
    -- Amazon SageMaker places no restrictions on their use.
    --
    -- -   ContributedTo - The source contributed to the destination or had a
    --     part in enabling the destination. For example, the training data
    --     contributed to the training job.
    --
    -- -   AssociatedWith - The source is connected to the destination. For
    --     example, an approval workflow is associated with a model deployment.
    --
    -- -   DerivedFrom - The destination is a modification of the source. For
    --     example, a digest output of a channel input for a processing job is
    --     derived from the original inputs.
    --
    -- -   Produced - The source generated the destination. For example, a
    --     training job produced a model artifact.
    associationType :: Prelude.Maybe AssociationEdgeType,
    -- | The ARN of the source.
    sourceArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationType', 'addAssociation_associationType' - The type of association. The following are suggested uses for each type.
-- Amazon SageMaker places no restrictions on their use.
--
-- -   ContributedTo - The source contributed to the destination or had a
--     part in enabling the destination. For example, the training data
--     contributed to the training job.
--
-- -   AssociatedWith - The source is connected to the destination. For
--     example, an approval workflow is associated with a model deployment.
--
-- -   DerivedFrom - The destination is a modification of the source. For
--     example, a digest output of a channel input for a processing job is
--     derived from the original inputs.
--
-- -   Produced - The source generated the destination. For example, a
--     training job produced a model artifact.
--
-- 'sourceArn', 'addAssociation_sourceArn' - The ARN of the source.
--
-- 'destinationArn', 'addAssociation_destinationArn' - The Amazon Resource Name (ARN) of the destination.
newAddAssociation ::
  -- | 'sourceArn'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  AddAssociation
newAddAssociation pSourceArn_ pDestinationArn_ =
  AddAssociation'
    { associationType = Prelude.Nothing,
      sourceArn = pSourceArn_,
      destinationArn = pDestinationArn_
    }

-- | The type of association. The following are suggested uses for each type.
-- Amazon SageMaker places no restrictions on their use.
--
-- -   ContributedTo - The source contributed to the destination or had a
--     part in enabling the destination. For example, the training data
--     contributed to the training job.
--
-- -   AssociatedWith - The source is connected to the destination. For
--     example, an approval workflow is associated with a model deployment.
--
-- -   DerivedFrom - The destination is a modification of the source. For
--     example, a digest output of a channel input for a processing job is
--     derived from the original inputs.
--
-- -   Produced - The source generated the destination. For example, a
--     training job produced a model artifact.
addAssociation_associationType :: Lens.Lens' AddAssociation (Prelude.Maybe AssociationEdgeType)
addAssociation_associationType = Lens.lens (\AddAssociation' {associationType} -> associationType) (\s@AddAssociation' {} a -> s {associationType = a} :: AddAssociation)

-- | The ARN of the source.
addAssociation_sourceArn :: Lens.Lens' AddAssociation Prelude.Text
addAssociation_sourceArn = Lens.lens (\AddAssociation' {sourceArn} -> sourceArn) (\s@AddAssociation' {} a -> s {sourceArn = a} :: AddAssociation)

-- | The Amazon Resource Name (ARN) of the destination.
addAssociation_destinationArn :: Lens.Lens' AddAssociation Prelude.Text
addAssociation_destinationArn = Lens.lens (\AddAssociation' {destinationArn} -> destinationArn) (\s@AddAssociation' {} a -> s {destinationArn = a} :: AddAssociation)

instance Core.AWSRequest AddAssociation where
  type
    AWSResponse AddAssociation =
      AddAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAssociationResponse'
            Prelude.<$> (x Data..?> "DestinationArn")
            Prelude.<*> (x Data..?> "SourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddAssociation where
  hashWithSalt _salt AddAssociation' {..} =
    _salt `Prelude.hashWithSalt` associationType
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` destinationArn

instance Prelude.NFData AddAssociation where
  rnf AddAssociation' {..} =
    Prelude.rnf associationType
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf destinationArn

instance Data.ToHeaders AddAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.AddAssociation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddAssociation where
  toJSON AddAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssociationType" Data..=)
              Prelude.<$> associationType,
            Prelude.Just ("SourceArn" Data..= sourceArn),
            Prelude.Just
              ("DestinationArn" Data..= destinationArn)
          ]
      )

instance Data.ToPath AddAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery AddAssociation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddAssociationResponse' smart constructor.
data AddAssociationResponse = AddAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationArn', 'addAssociationResponse_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'sourceArn', 'addAssociationResponse_sourceArn' - The ARN of the source.
--
-- 'httpStatus', 'addAssociationResponse_httpStatus' - The response's http status code.
newAddAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AddAssociationResponse
newAddAssociationResponse pHttpStatus_ =
  AddAssociationResponse'
    { destinationArn =
        Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the destination.
addAssociationResponse_destinationArn :: Lens.Lens' AddAssociationResponse (Prelude.Maybe Prelude.Text)
addAssociationResponse_destinationArn = Lens.lens (\AddAssociationResponse' {destinationArn} -> destinationArn) (\s@AddAssociationResponse' {} a -> s {destinationArn = a} :: AddAssociationResponse)

-- | The ARN of the source.
addAssociationResponse_sourceArn :: Lens.Lens' AddAssociationResponse (Prelude.Maybe Prelude.Text)
addAssociationResponse_sourceArn = Lens.lens (\AddAssociationResponse' {sourceArn} -> sourceArn) (\s@AddAssociationResponse' {} a -> s {sourceArn = a} :: AddAssociationResponse)

-- | The response's http status code.
addAssociationResponse_httpStatus :: Lens.Lens' AddAssociationResponse Prelude.Int
addAssociationResponse_httpStatus = Lens.lens (\AddAssociationResponse' {httpStatus} -> httpStatus) (\s@AddAssociationResponse' {} a -> s {httpStatus = a} :: AddAssociationResponse)

instance Prelude.NFData AddAssociationResponse where
  rnf AddAssociationResponse' {..} =
    Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf httpStatus

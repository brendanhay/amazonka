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
-- Module      : Network.AWS.SageMaker.AddAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SageMaker.AddAssociation
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AddAssociation where
  type Rs AddAssociation = AddAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AddAssociationResponse'
            Prelude.<$> (x Prelude..?> "DestinationArn")
            Prelude.<*> (x Prelude..?> "SourceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AddAssociation

instance Prelude.NFData AddAssociation

instance Prelude.ToHeaders AddAssociation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.AddAssociation" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AddAssociation where
  toJSON AddAssociation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AssociationType" Prelude..=)
              Prelude.<$> associationType,
            Prelude.Just ("SourceArn" Prelude..= sourceArn),
            Prelude.Just
              ("DestinationArn" Prelude..= destinationArn)
          ]
      )

instance Prelude.ToPath AddAssociation where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AddAssociation where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData AddAssociationResponse

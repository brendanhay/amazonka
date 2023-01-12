{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GuardDuty.Types.EksClusterDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.EksClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Details about the EKS cluster involved in a Kubernetes finding.
--
-- /See:/ 'newEksClusterDetails' smart constructor.
data EksClusterDetails = EksClusterDetails'
  { -- | EKS cluster ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the EKS cluster was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | EKS cluster name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The EKS cluster status.
    status :: Prelude.Maybe Prelude.Text,
    -- | The EKS cluster tags.
    tags :: Prelude.Maybe [Tag],
    -- | The VPC ID to which the EKS cluster is attached.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EksClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'eksClusterDetails_arn' - EKS cluster ARN.
--
-- 'createdAt', 'eksClusterDetails_createdAt' - The timestamp when the EKS cluster was created.
--
-- 'name', 'eksClusterDetails_name' - EKS cluster name.
--
-- 'status', 'eksClusterDetails_status' - The EKS cluster status.
--
-- 'tags', 'eksClusterDetails_tags' - The EKS cluster tags.
--
-- 'vpcId', 'eksClusterDetails_vpcId' - The VPC ID to which the EKS cluster is attached.
newEksClusterDetails ::
  EksClusterDetails
newEksClusterDetails =
  EksClusterDetails'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | EKS cluster ARN.
eksClusterDetails_arn :: Lens.Lens' EksClusterDetails (Prelude.Maybe Prelude.Text)
eksClusterDetails_arn = Lens.lens (\EksClusterDetails' {arn} -> arn) (\s@EksClusterDetails' {} a -> s {arn = a} :: EksClusterDetails)

-- | The timestamp when the EKS cluster was created.
eksClusterDetails_createdAt :: Lens.Lens' EksClusterDetails (Prelude.Maybe Prelude.UTCTime)
eksClusterDetails_createdAt = Lens.lens (\EksClusterDetails' {createdAt} -> createdAt) (\s@EksClusterDetails' {} a -> s {createdAt = a} :: EksClusterDetails) Prelude.. Lens.mapping Data._Time

-- | EKS cluster name.
eksClusterDetails_name :: Lens.Lens' EksClusterDetails (Prelude.Maybe Prelude.Text)
eksClusterDetails_name = Lens.lens (\EksClusterDetails' {name} -> name) (\s@EksClusterDetails' {} a -> s {name = a} :: EksClusterDetails)

-- | The EKS cluster status.
eksClusterDetails_status :: Lens.Lens' EksClusterDetails (Prelude.Maybe Prelude.Text)
eksClusterDetails_status = Lens.lens (\EksClusterDetails' {status} -> status) (\s@EksClusterDetails' {} a -> s {status = a} :: EksClusterDetails)

-- | The EKS cluster tags.
eksClusterDetails_tags :: Lens.Lens' EksClusterDetails (Prelude.Maybe [Tag])
eksClusterDetails_tags = Lens.lens (\EksClusterDetails' {tags} -> tags) (\s@EksClusterDetails' {} a -> s {tags = a} :: EksClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The VPC ID to which the EKS cluster is attached.
eksClusterDetails_vpcId :: Lens.Lens' EksClusterDetails (Prelude.Maybe Prelude.Text)
eksClusterDetails_vpcId = Lens.lens (\EksClusterDetails' {vpcId} -> vpcId) (\s@EksClusterDetails' {} a -> s {vpcId = a} :: EksClusterDetails)

instance Data.FromJSON EksClusterDetails where
  parseJSON =
    Data.withObject
      "EksClusterDetails"
      ( \x ->
          EksClusterDetails'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "vpcId")
      )

instance Prelude.Hashable EksClusterDetails where
  hashWithSalt _salt EksClusterDetails' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData EksClusterDetails where
  rnf EksClusterDetails' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf vpcId

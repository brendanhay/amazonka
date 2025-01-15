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
-- Module      : Amazonka.MediaConnect.Types.ListedFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.ListedFlow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConnect.Types.Maintenance
import Amazonka.MediaConnect.Types.SourceType
import Amazonka.MediaConnect.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of a flow, including its ARN, Availability Zone, and
-- source type.
--
-- /See:/ 'newListedFlow' smart constructor.
data ListedFlow = ListedFlow'
  { maintenance :: Prelude.Maybe Maintenance,
    -- | The current status of the flow.
    status :: Status,
    -- | A description of the flow.
    description :: Prelude.Text,
    -- | The type of source. This value is either owned (originated somewhere
    -- other than an AWS Elemental MediaConnect flow owned by another AWS
    -- account) or entitled (originated at an AWS Elemental MediaConnect flow
    -- owned by another AWS account).
    sourceType :: SourceType,
    -- | The Availability Zone that the flow was created in.
    availabilityZone :: Prelude.Text,
    -- | The ARN of the flow.
    flowArn :: Prelude.Text,
    -- | The name of the flow.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maintenance', 'listedFlow_maintenance' - Undocumented member.
--
-- 'status', 'listedFlow_status' - The current status of the flow.
--
-- 'description', 'listedFlow_description' - A description of the flow.
--
-- 'sourceType', 'listedFlow_sourceType' - The type of source. This value is either owned (originated somewhere
-- other than an AWS Elemental MediaConnect flow owned by another AWS
-- account) or entitled (originated at an AWS Elemental MediaConnect flow
-- owned by another AWS account).
--
-- 'availabilityZone', 'listedFlow_availabilityZone' - The Availability Zone that the flow was created in.
--
-- 'flowArn', 'listedFlow_flowArn' - The ARN of the flow.
--
-- 'name', 'listedFlow_name' - The name of the flow.
newListedFlow ::
  -- | 'status'
  Status ->
  -- | 'description'
  Prelude.Text ->
  -- | 'sourceType'
  SourceType ->
  -- | 'availabilityZone'
  Prelude.Text ->
  -- | 'flowArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ListedFlow
newListedFlow
  pStatus_
  pDescription_
  pSourceType_
  pAvailabilityZone_
  pFlowArn_
  pName_ =
    ListedFlow'
      { maintenance = Prelude.Nothing,
        status = pStatus_,
        description = pDescription_,
        sourceType = pSourceType_,
        availabilityZone = pAvailabilityZone_,
        flowArn = pFlowArn_,
        name = pName_
      }

-- | Undocumented member.
listedFlow_maintenance :: Lens.Lens' ListedFlow (Prelude.Maybe Maintenance)
listedFlow_maintenance = Lens.lens (\ListedFlow' {maintenance} -> maintenance) (\s@ListedFlow' {} a -> s {maintenance = a} :: ListedFlow)

-- | The current status of the flow.
listedFlow_status :: Lens.Lens' ListedFlow Status
listedFlow_status = Lens.lens (\ListedFlow' {status} -> status) (\s@ListedFlow' {} a -> s {status = a} :: ListedFlow)

-- | A description of the flow.
listedFlow_description :: Lens.Lens' ListedFlow Prelude.Text
listedFlow_description = Lens.lens (\ListedFlow' {description} -> description) (\s@ListedFlow' {} a -> s {description = a} :: ListedFlow)

-- | The type of source. This value is either owned (originated somewhere
-- other than an AWS Elemental MediaConnect flow owned by another AWS
-- account) or entitled (originated at an AWS Elemental MediaConnect flow
-- owned by another AWS account).
listedFlow_sourceType :: Lens.Lens' ListedFlow SourceType
listedFlow_sourceType = Lens.lens (\ListedFlow' {sourceType} -> sourceType) (\s@ListedFlow' {} a -> s {sourceType = a} :: ListedFlow)

-- | The Availability Zone that the flow was created in.
listedFlow_availabilityZone :: Lens.Lens' ListedFlow Prelude.Text
listedFlow_availabilityZone = Lens.lens (\ListedFlow' {availabilityZone} -> availabilityZone) (\s@ListedFlow' {} a -> s {availabilityZone = a} :: ListedFlow)

-- | The ARN of the flow.
listedFlow_flowArn :: Lens.Lens' ListedFlow Prelude.Text
listedFlow_flowArn = Lens.lens (\ListedFlow' {flowArn} -> flowArn) (\s@ListedFlow' {} a -> s {flowArn = a} :: ListedFlow)

-- | The name of the flow.
listedFlow_name :: Lens.Lens' ListedFlow Prelude.Text
listedFlow_name = Lens.lens (\ListedFlow' {name} -> name) (\s@ListedFlow' {} a -> s {name = a} :: ListedFlow)

instance Data.FromJSON ListedFlow where
  parseJSON =
    Data.withObject
      "ListedFlow"
      ( \x ->
          ListedFlow'
            Prelude.<$> (x Data..:? "maintenance")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "sourceType")
            Prelude.<*> (x Data..: "availabilityZone")
            Prelude.<*> (x Data..: "flowArn")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ListedFlow where
  hashWithSalt _salt ListedFlow' {..} =
    _salt
      `Prelude.hashWithSalt` maintenance
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sourceType
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` flowArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData ListedFlow where
  rnf ListedFlow' {..} =
    Prelude.rnf maintenance `Prelude.seq`
      Prelude.rnf status `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf sourceType `Prelude.seq`
            Prelude.rnf availabilityZone `Prelude.seq`
              Prelude.rnf flowArn `Prelude.seq`
                Prelude.rnf name

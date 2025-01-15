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
-- Module      : Amazonka.IotTwinMaker.Types.ComponentTypeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IotTwinMaker.Types.ComponentTypeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types.Status
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a component type.
--
-- /See:/ 'newComponentTypeSummary' smart constructor.
data ComponentTypeSummary = ComponentTypeSummary'
  { -- | The component type name.
    componentTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the component type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The current status of the component type.
    status :: Prelude.Maybe Status,
    -- | The ARN of the component type.
    arn :: Prelude.Text,
    -- | The ID of the component type.
    componentTypeId :: Prelude.Text,
    -- | The date and time when the component type was created.
    creationDateTime :: Data.POSIX,
    -- | The date and time when the component type was last updated.
    updateDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentTypeName', 'componentTypeSummary_componentTypeName' - The component type name.
--
-- 'description', 'componentTypeSummary_description' - The description of the component type.
--
-- 'status', 'componentTypeSummary_status' - The current status of the component type.
--
-- 'arn', 'componentTypeSummary_arn' - The ARN of the component type.
--
-- 'componentTypeId', 'componentTypeSummary_componentTypeId' - The ID of the component type.
--
-- 'creationDateTime', 'componentTypeSummary_creationDateTime' - The date and time when the component type was created.
--
-- 'updateDateTime', 'componentTypeSummary_updateDateTime' - The date and time when the component type was last updated.
newComponentTypeSummary ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'componentTypeId'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  ComponentTypeSummary
newComponentTypeSummary
  pArn_
  pComponentTypeId_
  pCreationDateTime_
  pUpdateDateTime_ =
    ComponentTypeSummary'
      { componentTypeName =
          Prelude.Nothing,
        description = Prelude.Nothing,
        status = Prelude.Nothing,
        arn = pArn_,
        componentTypeId = pComponentTypeId_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_
      }

-- | The component type name.
componentTypeSummary_componentTypeName :: Lens.Lens' ComponentTypeSummary (Prelude.Maybe Prelude.Text)
componentTypeSummary_componentTypeName = Lens.lens (\ComponentTypeSummary' {componentTypeName} -> componentTypeName) (\s@ComponentTypeSummary' {} a -> s {componentTypeName = a} :: ComponentTypeSummary)

-- | The description of the component type.
componentTypeSummary_description :: Lens.Lens' ComponentTypeSummary (Prelude.Maybe Prelude.Text)
componentTypeSummary_description = Lens.lens (\ComponentTypeSummary' {description} -> description) (\s@ComponentTypeSummary' {} a -> s {description = a} :: ComponentTypeSummary)

-- | The current status of the component type.
componentTypeSummary_status :: Lens.Lens' ComponentTypeSummary (Prelude.Maybe Status)
componentTypeSummary_status = Lens.lens (\ComponentTypeSummary' {status} -> status) (\s@ComponentTypeSummary' {} a -> s {status = a} :: ComponentTypeSummary)

-- | The ARN of the component type.
componentTypeSummary_arn :: Lens.Lens' ComponentTypeSummary Prelude.Text
componentTypeSummary_arn = Lens.lens (\ComponentTypeSummary' {arn} -> arn) (\s@ComponentTypeSummary' {} a -> s {arn = a} :: ComponentTypeSummary)

-- | The ID of the component type.
componentTypeSummary_componentTypeId :: Lens.Lens' ComponentTypeSummary Prelude.Text
componentTypeSummary_componentTypeId = Lens.lens (\ComponentTypeSummary' {componentTypeId} -> componentTypeId) (\s@ComponentTypeSummary' {} a -> s {componentTypeId = a} :: ComponentTypeSummary)

-- | The date and time when the component type was created.
componentTypeSummary_creationDateTime :: Lens.Lens' ComponentTypeSummary Prelude.UTCTime
componentTypeSummary_creationDateTime = Lens.lens (\ComponentTypeSummary' {creationDateTime} -> creationDateTime) (\s@ComponentTypeSummary' {} a -> s {creationDateTime = a} :: ComponentTypeSummary) Prelude.. Data._Time

-- | The date and time when the component type was last updated.
componentTypeSummary_updateDateTime :: Lens.Lens' ComponentTypeSummary Prelude.UTCTime
componentTypeSummary_updateDateTime = Lens.lens (\ComponentTypeSummary' {updateDateTime} -> updateDateTime) (\s@ComponentTypeSummary' {} a -> s {updateDateTime = a} :: ComponentTypeSummary) Prelude.. Data._Time

instance Data.FromJSON ComponentTypeSummary where
  parseJSON =
    Data.withObject
      "ComponentTypeSummary"
      ( \x ->
          ComponentTypeSummary'
            Prelude.<$> (x Data..:? "componentTypeName")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "componentTypeId")
            Prelude.<*> (x Data..: "creationDateTime")
            Prelude.<*> (x Data..: "updateDateTime")
      )

instance Prelude.Hashable ComponentTypeSummary where
  hashWithSalt _salt ComponentTypeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` componentTypeName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` componentTypeId
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` updateDateTime

instance Prelude.NFData ComponentTypeSummary where
  rnf ComponentTypeSummary' {..} =
    Prelude.rnf componentTypeName `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf arn `Prelude.seq`
            Prelude.rnf componentTypeId `Prelude.seq`
              Prelude.rnf creationDateTime `Prelude.seq`
                Prelude.rnf updateDateTime

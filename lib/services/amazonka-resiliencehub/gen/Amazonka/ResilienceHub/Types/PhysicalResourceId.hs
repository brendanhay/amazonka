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
-- Module      : Amazonka.ResilienceHub.Types.PhysicalResourceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.PhysicalResourceId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.PhysicalIdentifierType

-- | Defines a physical resource identifier.
--
-- /See:/ 'newPhysicalResourceId' smart constructor.
data PhysicalResourceId = PhysicalResourceId'
  { -- | The Amazon Web Services account that owns the physical resource.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that the physical resource is located in.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the physical resource.
    identifier :: Prelude.Text,
    -- | Specifies the type of physical resource identifier.
    --
    -- [Arn]
    --     The resource identifier is an Amazon Resource Name (ARN) .
    --
    -- [Native]
    --     The resource identifier is a Resilience Hub-native identifier.
    type' :: PhysicalIdentifierType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhysicalResourceId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'physicalResourceId_awsAccountId' - The Amazon Web Services account that owns the physical resource.
--
-- 'awsRegion', 'physicalResourceId_awsRegion' - The Amazon Web Services Region that the physical resource is located in.
--
-- 'identifier', 'physicalResourceId_identifier' - The identifier of the physical resource.
--
-- 'type'', 'physicalResourceId_type' - Specifies the type of physical resource identifier.
--
-- [Arn]
--     The resource identifier is an Amazon Resource Name (ARN) .
--
-- [Native]
--     The resource identifier is a Resilience Hub-native identifier.
newPhysicalResourceId ::
  -- | 'identifier'
  Prelude.Text ->
  -- | 'type''
  PhysicalIdentifierType ->
  PhysicalResourceId
newPhysicalResourceId pIdentifier_ pType_ =
  PhysicalResourceId'
    { awsAccountId = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      identifier = pIdentifier_,
      type' = pType_
    }

-- | The Amazon Web Services account that owns the physical resource.
physicalResourceId_awsAccountId :: Lens.Lens' PhysicalResourceId (Prelude.Maybe Prelude.Text)
physicalResourceId_awsAccountId = Lens.lens (\PhysicalResourceId' {awsAccountId} -> awsAccountId) (\s@PhysicalResourceId' {} a -> s {awsAccountId = a} :: PhysicalResourceId)

-- | The Amazon Web Services Region that the physical resource is located in.
physicalResourceId_awsRegion :: Lens.Lens' PhysicalResourceId (Prelude.Maybe Prelude.Text)
physicalResourceId_awsRegion = Lens.lens (\PhysicalResourceId' {awsRegion} -> awsRegion) (\s@PhysicalResourceId' {} a -> s {awsRegion = a} :: PhysicalResourceId)

-- | The identifier of the physical resource.
physicalResourceId_identifier :: Lens.Lens' PhysicalResourceId Prelude.Text
physicalResourceId_identifier = Lens.lens (\PhysicalResourceId' {identifier} -> identifier) (\s@PhysicalResourceId' {} a -> s {identifier = a} :: PhysicalResourceId)

-- | Specifies the type of physical resource identifier.
--
-- [Arn]
--     The resource identifier is an Amazon Resource Name (ARN) .
--
-- [Native]
--     The resource identifier is a Resilience Hub-native identifier.
physicalResourceId_type :: Lens.Lens' PhysicalResourceId PhysicalIdentifierType
physicalResourceId_type = Lens.lens (\PhysicalResourceId' {type'} -> type') (\s@PhysicalResourceId' {} a -> s {type' = a} :: PhysicalResourceId)

instance Data.FromJSON PhysicalResourceId where
  parseJSON =
    Data.withObject
      "PhysicalResourceId"
      ( \x ->
          PhysicalResourceId'
            Prelude.<$> (x Data..:? "awsAccountId")
            Prelude.<*> (x Data..:? "awsRegion")
            Prelude.<*> (x Data..: "identifier")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable PhysicalResourceId where
  hashWithSalt _salt PhysicalResourceId' {..} =
    _salt
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` identifier
      `Prelude.hashWithSalt` type'

instance Prelude.NFData PhysicalResourceId where
  rnf PhysicalResourceId' {..} =
    Prelude.rnf awsAccountId `Prelude.seq`
      Prelude.rnf awsRegion `Prelude.seq`
        Prelude.rnf identifier `Prelude.seq`
          Prelude.rnf type'

instance Data.ToJSON PhysicalResourceId where
  toJSON PhysicalResourceId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("awsAccountId" Data..=) Prelude.<$> awsAccountId,
            ("awsRegion" Data..=) Prelude.<$> awsRegion,
            Prelude.Just ("identifier" Data..= identifier),
            Prelude.Just ("type" Data..= type')
          ]
      )

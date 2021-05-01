{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServiceCatalog.Types.ServiceActionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ServiceActionSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ServiceActionDefinitionType

-- | Detailed information about the self-service action.
--
-- /See:/ 'newServiceActionSummary' smart constructor.
data ServiceActionSummary = ServiceActionSummary'
  { -- | The self-service action identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The self-service action definition type. For example, @SSM_AUTOMATION@.
    definitionType :: Prelude.Maybe ServiceActionDefinitionType,
    -- | The self-service action name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The self-service action description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ServiceActionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'serviceActionSummary_id' - The self-service action identifier.
--
-- 'definitionType', 'serviceActionSummary_definitionType' - The self-service action definition type. For example, @SSM_AUTOMATION@.
--
-- 'name', 'serviceActionSummary_name' - The self-service action name.
--
-- 'description', 'serviceActionSummary_description' - The self-service action description.
newServiceActionSummary ::
  ServiceActionSummary
newServiceActionSummary =
  ServiceActionSummary'
    { id = Prelude.Nothing,
      definitionType = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The self-service action identifier.
serviceActionSummary_id :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_id = Lens.lens (\ServiceActionSummary' {id} -> id) (\s@ServiceActionSummary' {} a -> s {id = a} :: ServiceActionSummary)

-- | The self-service action definition type. For example, @SSM_AUTOMATION@.
serviceActionSummary_definitionType :: Lens.Lens' ServiceActionSummary (Prelude.Maybe ServiceActionDefinitionType)
serviceActionSummary_definitionType = Lens.lens (\ServiceActionSummary' {definitionType} -> definitionType) (\s@ServiceActionSummary' {} a -> s {definitionType = a} :: ServiceActionSummary)

-- | The self-service action name.
serviceActionSummary_name :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_name = Lens.lens (\ServiceActionSummary' {name} -> name) (\s@ServiceActionSummary' {} a -> s {name = a} :: ServiceActionSummary)

-- | The self-service action description.
serviceActionSummary_description :: Lens.Lens' ServiceActionSummary (Prelude.Maybe Prelude.Text)
serviceActionSummary_description = Lens.lens (\ServiceActionSummary' {description} -> description) (\s@ServiceActionSummary' {} a -> s {description = a} :: ServiceActionSummary)

instance Prelude.FromJSON ServiceActionSummary where
  parseJSON =
    Prelude.withObject
      "ServiceActionSummary"
      ( \x ->
          ServiceActionSummary'
            Prelude.<$> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "DefinitionType")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable ServiceActionSummary

instance Prelude.NFData ServiceActionSummary

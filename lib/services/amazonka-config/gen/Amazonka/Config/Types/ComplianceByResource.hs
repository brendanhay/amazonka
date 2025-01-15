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
-- Module      : Amazonka.Config.Types.ComplianceByResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ComplianceByResource where

import Amazonka.Config.Types.Compliance
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Indicates whether an Amazon Web Services resource that is evaluated
-- according to one or more Config rules is compliant. A resource is
-- compliant if it complies with all of the rules that evaluate it. A
-- resource is noncompliant if it does not comply with one or more of these
-- rules.
--
-- /See:/ 'newComplianceByResource' smart constructor.
data ComplianceByResource = ComplianceByResource'
  { -- | Indicates whether the Amazon Web Services resource complies with all of
    -- the Config rules that evaluated it.
    compliance :: Prelude.Maybe Compliance,
    -- | The ID of the Amazon Web Services resource that was evaluated.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The type of the Amazon Web Services resource that was evaluated.
    resourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComplianceByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compliance', 'complianceByResource_compliance' - Indicates whether the Amazon Web Services resource complies with all of
-- the Config rules that evaluated it.
--
-- 'resourceId', 'complianceByResource_resourceId' - The ID of the Amazon Web Services resource that was evaluated.
--
-- 'resourceType', 'complianceByResource_resourceType' - The type of the Amazon Web Services resource that was evaluated.
newComplianceByResource ::
  ComplianceByResource
newComplianceByResource =
  ComplianceByResource'
    { compliance = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | Indicates whether the Amazon Web Services resource complies with all of
-- the Config rules that evaluated it.
complianceByResource_compliance :: Lens.Lens' ComplianceByResource (Prelude.Maybe Compliance)
complianceByResource_compliance = Lens.lens (\ComplianceByResource' {compliance} -> compliance) (\s@ComplianceByResource' {} a -> s {compliance = a} :: ComplianceByResource)

-- | The ID of the Amazon Web Services resource that was evaluated.
complianceByResource_resourceId :: Lens.Lens' ComplianceByResource (Prelude.Maybe Prelude.Text)
complianceByResource_resourceId = Lens.lens (\ComplianceByResource' {resourceId} -> resourceId) (\s@ComplianceByResource' {} a -> s {resourceId = a} :: ComplianceByResource)

-- | The type of the Amazon Web Services resource that was evaluated.
complianceByResource_resourceType :: Lens.Lens' ComplianceByResource (Prelude.Maybe Prelude.Text)
complianceByResource_resourceType = Lens.lens (\ComplianceByResource' {resourceType} -> resourceType) (\s@ComplianceByResource' {} a -> s {resourceType = a} :: ComplianceByResource)

instance Data.FromJSON ComplianceByResource where
  parseJSON =
    Data.withObject
      "ComplianceByResource"
      ( \x ->
          ComplianceByResource'
            Prelude.<$> (x Data..:? "Compliance")
            Prelude.<*> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "ResourceType")
      )

instance Prelude.Hashable ComplianceByResource where
  hashWithSalt _salt ComplianceByResource' {..} =
    _salt
      `Prelude.hashWithSalt` compliance
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ComplianceByResource where
  rnf ComplianceByResource' {..} =
    Prelude.rnf compliance `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf resourceType

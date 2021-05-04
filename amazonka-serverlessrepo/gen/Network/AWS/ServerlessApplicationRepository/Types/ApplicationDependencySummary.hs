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
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationDependencySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A nested application summary.
--
-- /See:/ 'newApplicationDependencySummary' smart constructor.
data ApplicationDependencySummary = ApplicationDependencySummary'
  { -- | The Amazon Resource Name (ARN) of the nested application.
    applicationId :: Prelude.Text,
    -- | The semantic version of the nested application.
    semanticVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApplicationDependencySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'applicationDependencySummary_applicationId' - The Amazon Resource Name (ARN) of the nested application.
--
-- 'semanticVersion', 'applicationDependencySummary_semanticVersion' - The semantic version of the nested application.
newApplicationDependencySummary ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  ApplicationDependencySummary
newApplicationDependencySummary
  pApplicationId_
  pSemanticVersion_ =
    ApplicationDependencySummary'
      { applicationId =
          pApplicationId_,
        semanticVersion = pSemanticVersion_
      }

-- | The Amazon Resource Name (ARN) of the nested application.
applicationDependencySummary_applicationId :: Lens.Lens' ApplicationDependencySummary Prelude.Text
applicationDependencySummary_applicationId = Lens.lens (\ApplicationDependencySummary' {applicationId} -> applicationId) (\s@ApplicationDependencySummary' {} a -> s {applicationId = a} :: ApplicationDependencySummary)

-- | The semantic version of the nested application.
applicationDependencySummary_semanticVersion :: Lens.Lens' ApplicationDependencySummary Prelude.Text
applicationDependencySummary_semanticVersion = Lens.lens (\ApplicationDependencySummary' {semanticVersion} -> semanticVersion) (\s@ApplicationDependencySummary' {} a -> s {semanticVersion = a} :: ApplicationDependencySummary)

instance
  Prelude.FromJSON
    ApplicationDependencySummary
  where
  parseJSON =
    Prelude.withObject
      "ApplicationDependencySummary"
      ( \x ->
          ApplicationDependencySummary'
            Prelude.<$> (x Prelude..: "applicationId")
            Prelude.<*> (x Prelude..: "semanticVersion")
      )

instance
  Prelude.Hashable
    ApplicationDependencySummary

instance Prelude.NFData ApplicationDependencySummary

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
-- Module      : Network.AWS.CloudSearch.Types.AnalysisSchemeStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.AnalysisSchemeStatus where

import Network.AWS.CloudSearch.Types.AnalysisScheme
import Network.AWS.CloudSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The status and configuration of an @AnalysisScheme@.
--
-- /See:/ 'newAnalysisSchemeStatus' smart constructor.
data AnalysisSchemeStatus = AnalysisSchemeStatus'
  { options :: AnalysisScheme,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSchemeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'analysisSchemeStatus_options' - Undocumented member.
--
-- 'status', 'analysisSchemeStatus_status' - Undocumented member.
newAnalysisSchemeStatus ::
  -- | 'options'
  AnalysisScheme ->
  -- | 'status'
  OptionStatus ->
  AnalysisSchemeStatus
newAnalysisSchemeStatus pOptions_ pStatus_ =
  AnalysisSchemeStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Undocumented member.
analysisSchemeStatus_options :: Lens.Lens' AnalysisSchemeStatus AnalysisScheme
analysisSchemeStatus_options = Lens.lens (\AnalysisSchemeStatus' {options} -> options) (\s@AnalysisSchemeStatus' {} a -> s {options = a} :: AnalysisSchemeStatus)

-- | Undocumented member.
analysisSchemeStatus_status :: Lens.Lens' AnalysisSchemeStatus OptionStatus
analysisSchemeStatus_status = Lens.lens (\AnalysisSchemeStatus' {status} -> status) (\s@AnalysisSchemeStatus' {} a -> s {status = a} :: AnalysisSchemeStatus)

instance Prelude.FromXML AnalysisSchemeStatus where
  parseXML x =
    AnalysisSchemeStatus'
      Prelude.<$> (x Prelude..@ "Options")
      Prelude.<*> (x Prelude..@ "Status")

instance Prelude.Hashable AnalysisSchemeStatus

instance Prelude.NFData AnalysisSchemeStatus

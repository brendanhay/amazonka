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
-- Module      : Network.AWS.CloudSearch.Types.SuggesterStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.SuggesterStatus where

import Network.AWS.CloudSearch.Types.OptionStatus
import Network.AWS.CloudSearch.Types.Suggester
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The value of a @Suggester@ and its current status.
--
-- /See:/ 'newSuggesterStatus' smart constructor.
data SuggesterStatus = SuggesterStatus'
  { options :: Suggester,
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SuggesterStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'suggesterStatus_options' - Undocumented member.
--
-- 'status', 'suggesterStatus_status' - Undocumented member.
newSuggesterStatus ::
  -- | 'options'
  Suggester ->
  -- | 'status'
  OptionStatus ->
  SuggesterStatus
newSuggesterStatus pOptions_ pStatus_ =
  SuggesterStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Undocumented member.
suggesterStatus_options :: Lens.Lens' SuggesterStatus Suggester
suggesterStatus_options = Lens.lens (\SuggesterStatus' {options} -> options) (\s@SuggesterStatus' {} a -> s {options = a} :: SuggesterStatus)

-- | Undocumented member.
suggesterStatus_status :: Lens.Lens' SuggesterStatus OptionStatus
suggesterStatus_status = Lens.lens (\SuggesterStatus' {status} -> status) (\s@SuggesterStatus' {} a -> s {status = a} :: SuggesterStatus)

instance Prelude.FromXML SuggesterStatus where
  parseXML x =
    SuggesterStatus'
      Prelude.<$> (x Prelude..@ "Options")
      Prelude.<*> (x Prelude..@ "Status")

instance Prelude.Hashable SuggesterStatus

instance Prelude.NFData SuggesterStatus

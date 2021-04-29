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
-- Module      : Network.AWS.Redshift.Types.SupportedPlatform
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.SupportedPlatform where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal

-- | A list of supported platforms for orderable clusters.
--
-- /See:/ 'newSupportedPlatform' smart constructor.
data SupportedPlatform = SupportedPlatform'
  { name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SupportedPlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'supportedPlatform_name' -
newSupportedPlatform ::
  SupportedPlatform
newSupportedPlatform =
  SupportedPlatform' {name = Prelude.Nothing}

-- |
supportedPlatform_name :: Lens.Lens' SupportedPlatform (Prelude.Maybe Prelude.Text)
supportedPlatform_name = Lens.lens (\SupportedPlatform' {name} -> name) (\s@SupportedPlatform' {} a -> s {name = a} :: SupportedPlatform)

instance Prelude.FromXML SupportedPlatform where
  parseXML x =
    SupportedPlatform'
      Prelude.<$> (x Prelude..@? "Name")

instance Prelude.Hashable SupportedPlatform

instance Prelude.NFData SupportedPlatform

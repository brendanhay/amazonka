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
-- Module      : Network.AWS.SMS.Types.Source
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.Source where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SMS.Types.S3Location

-- | Contains the location of a validation script.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { s3Location :: Prelude.Maybe S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'source_s3Location' - Undocumented member.
newSource ::
  Source
newSource = Source' {s3Location = Prelude.Nothing}

-- | Undocumented member.
source_s3Location :: Lens.Lens' Source (Prelude.Maybe S3Location)
source_s3Location = Lens.lens (\Source' {s3Location} -> s3Location) (\s@Source' {} a -> s {s3Location = a} :: Source)

instance Prelude.FromJSON Source where
  parseJSON =
    Prelude.withObject
      "Source"
      ( \x ->
          Source' Prelude.<$> (x Prelude..:? "s3Location")
      )

instance Prelude.Hashable Source

instance Prelude.NFData Source

instance Prelude.ToJSON Source where
  toJSON Source' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("s3Location" Prelude..=) Prelude.<$> s3Location]
      )

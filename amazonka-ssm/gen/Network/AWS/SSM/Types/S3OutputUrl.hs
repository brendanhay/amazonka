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
-- Module      : Network.AWS.SSM.Types.S3OutputUrl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.S3OutputUrl where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A URL for the S3 bucket where you want to store the results of this
-- request.
--
-- /See:/ 'newS3OutputUrl' smart constructor.
data S3OutputUrl = S3OutputUrl'
  { -- | A URL for an S3 bucket where you want to store the results of this
    -- request.
    outputUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'S3OutputUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputUrl', 's3OutputUrl_outputUrl' - A URL for an S3 bucket where you want to store the results of this
-- request.
newS3OutputUrl ::
  S3OutputUrl
newS3OutputUrl =
  S3OutputUrl' {outputUrl = Prelude.Nothing}

-- | A URL for an S3 bucket where you want to store the results of this
-- request.
s3OutputUrl_outputUrl :: Lens.Lens' S3OutputUrl (Prelude.Maybe Prelude.Text)
s3OutputUrl_outputUrl = Lens.lens (\S3OutputUrl' {outputUrl} -> outputUrl) (\s@S3OutputUrl' {} a -> s {outputUrl = a} :: S3OutputUrl)

instance Prelude.FromJSON S3OutputUrl where
  parseJSON =
    Prelude.withObject
      "S3OutputUrl"
      ( \x ->
          S3OutputUrl' Prelude.<$> (x Prelude..:? "OutputUrl")
      )

instance Prelude.Hashable S3OutputUrl

instance Prelude.NFData S3OutputUrl

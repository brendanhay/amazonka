{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.WellArchitected.GetLensVersionDifference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get lens version differences.
module Amazonka.WellArchitected.GetLensVersionDifference
  ( -- * Creating a Request
    GetLensVersionDifference (..),
    newGetLensVersionDifference,

    -- * Request Lenses
    getLensVersionDifference_baseLensVersion,
    getLensVersionDifference_targetLensVersion,
    getLensVersionDifference_lensAlias,

    -- * Destructuring the Response
    GetLensVersionDifferenceResponse (..),
    newGetLensVersionDifferenceResponse,

    -- * Response Lenses
    getLensVersionDifferenceResponse_baseLensVersion,
    getLensVersionDifferenceResponse_latestLensVersion,
    getLensVersionDifferenceResponse_lensAlias,
    getLensVersionDifferenceResponse_lensArn,
    getLensVersionDifferenceResponse_targetLensVersion,
    getLensVersionDifferenceResponse_versionDifferences,
    getLensVersionDifferenceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newGetLensVersionDifference' smart constructor.
data GetLensVersionDifference = GetLensVersionDifference'
  { -- | The base version of the lens.
    baseLensVersion :: Prelude.Maybe Prelude.Text,
    -- | The lens version to target a difference for.
    targetLensVersion :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensVersionDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseLensVersion', 'getLensVersionDifference_baseLensVersion' - The base version of the lens.
--
-- 'targetLensVersion', 'getLensVersionDifference_targetLensVersion' - The lens version to target a difference for.
--
-- 'lensAlias', 'getLensVersionDifference_lensAlias' - Undocumented member.
newGetLensVersionDifference ::
  -- | 'lensAlias'
  Prelude.Text ->
  GetLensVersionDifference
newGetLensVersionDifference pLensAlias_ =
  GetLensVersionDifference'
    { baseLensVersion =
        Prelude.Nothing,
      targetLensVersion = Prelude.Nothing,
      lensAlias = pLensAlias_
    }

-- | The base version of the lens.
getLensVersionDifference_baseLensVersion :: Lens.Lens' GetLensVersionDifference (Prelude.Maybe Prelude.Text)
getLensVersionDifference_baseLensVersion = Lens.lens (\GetLensVersionDifference' {baseLensVersion} -> baseLensVersion) (\s@GetLensVersionDifference' {} a -> s {baseLensVersion = a} :: GetLensVersionDifference)

-- | The lens version to target a difference for.
getLensVersionDifference_targetLensVersion :: Lens.Lens' GetLensVersionDifference (Prelude.Maybe Prelude.Text)
getLensVersionDifference_targetLensVersion = Lens.lens (\GetLensVersionDifference' {targetLensVersion} -> targetLensVersion) (\s@GetLensVersionDifference' {} a -> s {targetLensVersion = a} :: GetLensVersionDifference)

-- | Undocumented member.
getLensVersionDifference_lensAlias :: Lens.Lens' GetLensVersionDifference Prelude.Text
getLensVersionDifference_lensAlias = Lens.lens (\GetLensVersionDifference' {lensAlias} -> lensAlias) (\s@GetLensVersionDifference' {} a -> s {lensAlias = a} :: GetLensVersionDifference)

instance Core.AWSRequest GetLensVersionDifference where
  type
    AWSResponse GetLensVersionDifference =
      GetLensVersionDifferenceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLensVersionDifferenceResponse'
            Prelude.<$> (x Data..?> "BaseLensVersion")
            Prelude.<*> (x Data..?> "LatestLensVersion")
            Prelude.<*> (x Data..?> "LensAlias")
            Prelude.<*> (x Data..?> "LensArn")
            Prelude.<*> (x Data..?> "TargetLensVersion")
            Prelude.<*> (x Data..?> "VersionDifferences")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLensVersionDifference where
  hashWithSalt _salt GetLensVersionDifference' {..} =
    _salt
      `Prelude.hashWithSalt` baseLensVersion
      `Prelude.hashWithSalt` targetLensVersion
      `Prelude.hashWithSalt` lensAlias

instance Prelude.NFData GetLensVersionDifference where
  rnf GetLensVersionDifference' {..} =
    Prelude.rnf baseLensVersion
      `Prelude.seq` Prelude.rnf targetLensVersion
      `Prelude.seq` Prelude.rnf lensAlias

instance Data.ToHeaders GetLensVersionDifference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLensVersionDifference where
  toPath GetLensVersionDifference' {..} =
    Prelude.mconcat
      [ "/lenses/",
        Data.toBS lensAlias,
        "/versionDifference"
      ]

instance Data.ToQuery GetLensVersionDifference where
  toQuery GetLensVersionDifference' {..} =
    Prelude.mconcat
      [ "BaseLensVersion" Data.=: baseLensVersion,
        "TargetLensVersion" Data.=: targetLensVersion
      ]

-- | /See:/ 'newGetLensVersionDifferenceResponse' smart constructor.
data GetLensVersionDifferenceResponse = GetLensVersionDifferenceResponse'
  { -- | The base version of the lens.
    baseLensVersion :: Prelude.Maybe Prelude.Text,
    -- | The latest version of the lens.
    latestLensVersion :: Prelude.Maybe Prelude.Text,
    lensAlias :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the lens.
    lensArn :: Prelude.Maybe Prelude.Text,
    -- | The target lens version for the lens.
    targetLensVersion :: Prelude.Maybe Prelude.Text,
    versionDifferences :: Prelude.Maybe VersionDifferences,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLensVersionDifferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseLensVersion', 'getLensVersionDifferenceResponse_baseLensVersion' - The base version of the lens.
--
-- 'latestLensVersion', 'getLensVersionDifferenceResponse_latestLensVersion' - The latest version of the lens.
--
-- 'lensAlias', 'getLensVersionDifferenceResponse_lensAlias' - Undocumented member.
--
-- 'lensArn', 'getLensVersionDifferenceResponse_lensArn' - The ARN for the lens.
--
-- 'targetLensVersion', 'getLensVersionDifferenceResponse_targetLensVersion' - The target lens version for the lens.
--
-- 'versionDifferences', 'getLensVersionDifferenceResponse_versionDifferences' - Undocumented member.
--
-- 'httpStatus', 'getLensVersionDifferenceResponse_httpStatus' - The response's http status code.
newGetLensVersionDifferenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLensVersionDifferenceResponse
newGetLensVersionDifferenceResponse pHttpStatus_ =
  GetLensVersionDifferenceResponse'
    { baseLensVersion =
        Prelude.Nothing,
      latestLensVersion = Prelude.Nothing,
      lensAlias = Prelude.Nothing,
      lensArn = Prelude.Nothing,
      targetLensVersion = Prelude.Nothing,
      versionDifferences = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The base version of the lens.
getLensVersionDifferenceResponse_baseLensVersion :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe Prelude.Text)
getLensVersionDifferenceResponse_baseLensVersion = Lens.lens (\GetLensVersionDifferenceResponse' {baseLensVersion} -> baseLensVersion) (\s@GetLensVersionDifferenceResponse' {} a -> s {baseLensVersion = a} :: GetLensVersionDifferenceResponse)

-- | The latest version of the lens.
getLensVersionDifferenceResponse_latestLensVersion :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe Prelude.Text)
getLensVersionDifferenceResponse_latestLensVersion = Lens.lens (\GetLensVersionDifferenceResponse' {latestLensVersion} -> latestLensVersion) (\s@GetLensVersionDifferenceResponse' {} a -> s {latestLensVersion = a} :: GetLensVersionDifferenceResponse)

-- | Undocumented member.
getLensVersionDifferenceResponse_lensAlias :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe Prelude.Text)
getLensVersionDifferenceResponse_lensAlias = Lens.lens (\GetLensVersionDifferenceResponse' {lensAlias} -> lensAlias) (\s@GetLensVersionDifferenceResponse' {} a -> s {lensAlias = a} :: GetLensVersionDifferenceResponse)

-- | The ARN for the lens.
getLensVersionDifferenceResponse_lensArn :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe Prelude.Text)
getLensVersionDifferenceResponse_lensArn = Lens.lens (\GetLensVersionDifferenceResponse' {lensArn} -> lensArn) (\s@GetLensVersionDifferenceResponse' {} a -> s {lensArn = a} :: GetLensVersionDifferenceResponse)

-- | The target lens version for the lens.
getLensVersionDifferenceResponse_targetLensVersion :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe Prelude.Text)
getLensVersionDifferenceResponse_targetLensVersion = Lens.lens (\GetLensVersionDifferenceResponse' {targetLensVersion} -> targetLensVersion) (\s@GetLensVersionDifferenceResponse' {} a -> s {targetLensVersion = a} :: GetLensVersionDifferenceResponse)

-- | Undocumented member.
getLensVersionDifferenceResponse_versionDifferences :: Lens.Lens' GetLensVersionDifferenceResponse (Prelude.Maybe VersionDifferences)
getLensVersionDifferenceResponse_versionDifferences = Lens.lens (\GetLensVersionDifferenceResponse' {versionDifferences} -> versionDifferences) (\s@GetLensVersionDifferenceResponse' {} a -> s {versionDifferences = a} :: GetLensVersionDifferenceResponse)

-- | The response's http status code.
getLensVersionDifferenceResponse_httpStatus :: Lens.Lens' GetLensVersionDifferenceResponse Prelude.Int
getLensVersionDifferenceResponse_httpStatus = Lens.lens (\GetLensVersionDifferenceResponse' {httpStatus} -> httpStatus) (\s@GetLensVersionDifferenceResponse' {} a -> s {httpStatus = a} :: GetLensVersionDifferenceResponse)

instance
  Prelude.NFData
    GetLensVersionDifferenceResponse
  where
  rnf GetLensVersionDifferenceResponse' {..} =
    Prelude.rnf baseLensVersion
      `Prelude.seq` Prelude.rnf latestLensVersion
      `Prelude.seq` Prelude.rnf lensAlias
      `Prelude.seq` Prelude.rnf lensArn
      `Prelude.seq` Prelude.rnf targetLensVersion
      `Prelude.seq` Prelude.rnf versionDifferences
      `Prelude.seq` Prelude.rnf httpStatus

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
-- Module      : Amazonka.AccessAnalyzer.StartResourceScan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Immediately starts a scan of the policies applied to the specified
-- resource.
module Amazonka.AccessAnalyzer.StartResourceScan
  ( -- * Creating a Request
    StartResourceScan (..),
    newStartResourceScan,

    -- * Request Lenses
    startResourceScan_resourceOwnerAccount,
    startResourceScan_analyzerArn,
    startResourceScan_resourceArn,

    -- * Destructuring the Response
    StartResourceScanResponse (..),
    newStartResourceScanResponse,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Starts a scan of the policies applied to the specified resource.
--
-- /See:/ 'newStartResourceScan' smart constructor.
data StartResourceScan = StartResourceScan'
  { -- | The Amazon Web Services account ID that owns the resource. For most
    -- Amazon Web Services resources, the owning account is the account in
    -- which the resource was created.
    resourceOwnerAccount :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
    -- to use to scan the policies applied to the specified resource.
    analyzerArn :: Prelude.Text,
    -- | The ARN of the resource to scan.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartResourceScan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceOwnerAccount', 'startResourceScan_resourceOwnerAccount' - The Amazon Web Services account ID that owns the resource. For most
-- Amazon Web Services resources, the owning account is the account in
-- which the resource was created.
--
-- 'analyzerArn', 'startResourceScan_analyzerArn' - The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- to use to scan the policies applied to the specified resource.
--
-- 'resourceArn', 'startResourceScan_resourceArn' - The ARN of the resource to scan.
newStartResourceScan ::
  -- | 'analyzerArn'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  StartResourceScan
newStartResourceScan pAnalyzerArn_ pResourceArn_ =
  StartResourceScan'
    { resourceOwnerAccount =
        Prelude.Nothing,
      analyzerArn = pAnalyzerArn_,
      resourceArn = pResourceArn_
    }

-- | The Amazon Web Services account ID that owns the resource. For most
-- Amazon Web Services resources, the owning account is the account in
-- which the resource was created.
startResourceScan_resourceOwnerAccount :: Lens.Lens' StartResourceScan (Prelude.Maybe Prelude.Text)
startResourceScan_resourceOwnerAccount = Lens.lens (\StartResourceScan' {resourceOwnerAccount} -> resourceOwnerAccount) (\s@StartResourceScan' {} a -> s {resourceOwnerAccount = a} :: StartResourceScan)

-- | The
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-getting-started.html#permission-resources ARN of the analyzer>
-- to use to scan the policies applied to the specified resource.
startResourceScan_analyzerArn :: Lens.Lens' StartResourceScan Prelude.Text
startResourceScan_analyzerArn = Lens.lens (\StartResourceScan' {analyzerArn} -> analyzerArn) (\s@StartResourceScan' {} a -> s {analyzerArn = a} :: StartResourceScan)

-- | The ARN of the resource to scan.
startResourceScan_resourceArn :: Lens.Lens' StartResourceScan Prelude.Text
startResourceScan_resourceArn = Lens.lens (\StartResourceScan' {resourceArn} -> resourceArn) (\s@StartResourceScan' {} a -> s {resourceArn = a} :: StartResourceScan)

instance Core.AWSRequest StartResourceScan where
  type
    AWSResponse StartResourceScan =
      StartResourceScanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StartResourceScanResponse'

instance Prelude.Hashable StartResourceScan where
  hashWithSalt _salt StartResourceScan' {..} =
    _salt
      `Prelude.hashWithSalt` resourceOwnerAccount
      `Prelude.hashWithSalt` analyzerArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData StartResourceScan where
  rnf StartResourceScan' {..} =
    Prelude.rnf resourceOwnerAccount
      `Prelude.seq` Prelude.rnf analyzerArn
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders StartResourceScan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartResourceScan where
  toJSON StartResourceScan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("resourceOwnerAccount" Data..=)
              Prelude.<$> resourceOwnerAccount,
            Prelude.Just ("analyzerArn" Data..= analyzerArn),
            Prelude.Just ("resourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath StartResourceScan where
  toPath = Prelude.const "/resource/scan"

instance Data.ToQuery StartResourceScan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartResourceScanResponse' smart constructor.
data StartResourceScanResponse = StartResourceScanResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartResourceScanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStartResourceScanResponse ::
  StartResourceScanResponse
newStartResourceScanResponse =
  StartResourceScanResponse'

instance Prelude.NFData StartResourceScanResponse where
  rnf _ = ()

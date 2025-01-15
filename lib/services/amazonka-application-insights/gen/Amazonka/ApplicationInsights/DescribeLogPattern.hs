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
-- Module      : Amazonka.ApplicationInsights.DescribeLogPattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a specific log pattern from a @LogPatternSet@.
module Amazonka.ApplicationInsights.DescribeLogPattern
  ( -- * Creating a Request
    DescribeLogPattern (..),
    newDescribeLogPattern,

    -- * Request Lenses
    describeLogPattern_resourceGroupName,
    describeLogPattern_patternSetName,
    describeLogPattern_patternName,

    -- * Destructuring the Response
    DescribeLogPatternResponse (..),
    newDescribeLogPatternResponse,

    -- * Response Lenses
    describeLogPatternResponse_logPattern,
    describeLogPatternResponse_resourceGroupName,
    describeLogPatternResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLogPattern' smart constructor.
data DescribeLogPattern = DescribeLogPattern'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the log pattern set.
    patternSetName :: Prelude.Text,
    -- | The name of the log pattern.
    patternName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'describeLogPattern_resourceGroupName' - The name of the resource group.
--
-- 'patternSetName', 'describeLogPattern_patternSetName' - The name of the log pattern set.
--
-- 'patternName', 'describeLogPattern_patternName' - The name of the log pattern.
newDescribeLogPattern ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'patternSetName'
  Prelude.Text ->
  -- | 'patternName'
  Prelude.Text ->
  DescribeLogPattern
newDescribeLogPattern
  pResourceGroupName_
  pPatternSetName_
  pPatternName_ =
    DescribeLogPattern'
      { resourceGroupName =
          pResourceGroupName_,
        patternSetName = pPatternSetName_,
        patternName = pPatternName_
      }

-- | The name of the resource group.
describeLogPattern_resourceGroupName :: Lens.Lens' DescribeLogPattern Prelude.Text
describeLogPattern_resourceGroupName = Lens.lens (\DescribeLogPattern' {resourceGroupName} -> resourceGroupName) (\s@DescribeLogPattern' {} a -> s {resourceGroupName = a} :: DescribeLogPattern)

-- | The name of the log pattern set.
describeLogPattern_patternSetName :: Lens.Lens' DescribeLogPattern Prelude.Text
describeLogPattern_patternSetName = Lens.lens (\DescribeLogPattern' {patternSetName} -> patternSetName) (\s@DescribeLogPattern' {} a -> s {patternSetName = a} :: DescribeLogPattern)

-- | The name of the log pattern.
describeLogPattern_patternName :: Lens.Lens' DescribeLogPattern Prelude.Text
describeLogPattern_patternName = Lens.lens (\DescribeLogPattern' {patternName} -> patternName) (\s@DescribeLogPattern' {} a -> s {patternName = a} :: DescribeLogPattern)

instance Core.AWSRequest DescribeLogPattern where
  type
    AWSResponse DescribeLogPattern =
      DescribeLogPatternResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLogPatternResponse'
            Prelude.<$> (x Data..?> "LogPattern")
            Prelude.<*> (x Data..?> "ResourceGroupName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLogPattern where
  hashWithSalt _salt DescribeLogPattern' {..} =
    _salt
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` patternSetName
      `Prelude.hashWithSalt` patternName

instance Prelude.NFData DescribeLogPattern where
  rnf DescribeLogPattern' {..} =
    Prelude.rnf resourceGroupName `Prelude.seq`
      Prelude.rnf patternSetName `Prelude.seq`
        Prelude.rnf patternName

instance Data.ToHeaders DescribeLogPattern where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DescribeLogPattern" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLogPattern where
  toJSON DescribeLogPattern' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("PatternSetName" Data..= patternSetName),
            Prelude.Just ("PatternName" Data..= patternName)
          ]
      )

instance Data.ToPath DescribeLogPattern where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLogPattern where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLogPatternResponse' smart constructor.
data DescribeLogPatternResponse = DescribeLogPatternResponse'
  { -- | The successfully created log pattern.
    logPattern :: Prelude.Maybe LogPattern,
    -- | The name of the resource group.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLogPatternResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logPattern', 'describeLogPatternResponse_logPattern' - The successfully created log pattern.
--
-- 'resourceGroupName', 'describeLogPatternResponse_resourceGroupName' - The name of the resource group.
--
-- 'httpStatus', 'describeLogPatternResponse_httpStatus' - The response's http status code.
newDescribeLogPatternResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLogPatternResponse
newDescribeLogPatternResponse pHttpStatus_ =
  DescribeLogPatternResponse'
    { logPattern =
        Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The successfully created log pattern.
describeLogPatternResponse_logPattern :: Lens.Lens' DescribeLogPatternResponse (Prelude.Maybe LogPattern)
describeLogPatternResponse_logPattern = Lens.lens (\DescribeLogPatternResponse' {logPattern} -> logPattern) (\s@DescribeLogPatternResponse' {} a -> s {logPattern = a} :: DescribeLogPatternResponse)

-- | The name of the resource group.
describeLogPatternResponse_resourceGroupName :: Lens.Lens' DescribeLogPatternResponse (Prelude.Maybe Prelude.Text)
describeLogPatternResponse_resourceGroupName = Lens.lens (\DescribeLogPatternResponse' {resourceGroupName} -> resourceGroupName) (\s@DescribeLogPatternResponse' {} a -> s {resourceGroupName = a} :: DescribeLogPatternResponse)

-- | The response's http status code.
describeLogPatternResponse_httpStatus :: Lens.Lens' DescribeLogPatternResponse Prelude.Int
describeLogPatternResponse_httpStatus = Lens.lens (\DescribeLogPatternResponse' {httpStatus} -> httpStatus) (\s@DescribeLogPatternResponse' {} a -> s {httpStatus = a} :: DescribeLogPatternResponse)

instance Prelude.NFData DescribeLogPatternResponse where
  rnf DescribeLogPatternResponse' {..} =
    Prelude.rnf logPattern `Prelude.seq`
      Prelude.rnf resourceGroupName `Prelude.seq`
        Prelude.rnf httpStatus

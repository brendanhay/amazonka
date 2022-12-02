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
-- Module      : Amazonka.ApplicationInsights.DeleteLogPattern
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified log pattern from a @LogPatternSet@.
module Amazonka.ApplicationInsights.DeleteLogPattern
  ( -- * Creating a Request
    DeleteLogPattern (..),
    newDeleteLogPattern,

    -- * Request Lenses
    deleteLogPattern_resourceGroupName,
    deleteLogPattern_patternSetName,
    deleteLogPattern_patternName,

    -- * Destructuring the Response
    DeleteLogPatternResponse (..),
    newDeleteLogPatternResponse,

    -- * Response Lenses
    deleteLogPatternResponse_httpStatus,
  )
where

import Amazonka.ApplicationInsights.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLogPattern' smart constructor.
data DeleteLogPattern = DeleteLogPattern'
  { -- | The name of the resource group.
    resourceGroupName :: Prelude.Text,
    -- | The name of the log pattern set.
    patternSetName :: Prelude.Text,
    -- | The name of the log pattern.
    patternName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceGroupName', 'deleteLogPattern_resourceGroupName' - The name of the resource group.
--
-- 'patternSetName', 'deleteLogPattern_patternSetName' - The name of the log pattern set.
--
-- 'patternName', 'deleteLogPattern_patternName' - The name of the log pattern.
newDeleteLogPattern ::
  -- | 'resourceGroupName'
  Prelude.Text ->
  -- | 'patternSetName'
  Prelude.Text ->
  -- | 'patternName'
  Prelude.Text ->
  DeleteLogPattern
newDeleteLogPattern
  pResourceGroupName_
  pPatternSetName_
  pPatternName_ =
    DeleteLogPattern'
      { resourceGroupName =
          pResourceGroupName_,
        patternSetName = pPatternSetName_,
        patternName = pPatternName_
      }

-- | The name of the resource group.
deleteLogPattern_resourceGroupName :: Lens.Lens' DeleteLogPattern Prelude.Text
deleteLogPattern_resourceGroupName = Lens.lens (\DeleteLogPattern' {resourceGroupName} -> resourceGroupName) (\s@DeleteLogPattern' {} a -> s {resourceGroupName = a} :: DeleteLogPattern)

-- | The name of the log pattern set.
deleteLogPattern_patternSetName :: Lens.Lens' DeleteLogPattern Prelude.Text
deleteLogPattern_patternSetName = Lens.lens (\DeleteLogPattern' {patternSetName} -> patternSetName) (\s@DeleteLogPattern' {} a -> s {patternSetName = a} :: DeleteLogPattern)

-- | The name of the log pattern.
deleteLogPattern_patternName :: Lens.Lens' DeleteLogPattern Prelude.Text
deleteLogPattern_patternName = Lens.lens (\DeleteLogPattern' {patternName} -> patternName) (\s@DeleteLogPattern' {} a -> s {patternName = a} :: DeleteLogPattern)

instance Core.AWSRequest DeleteLogPattern where
  type
    AWSResponse DeleteLogPattern =
      DeleteLogPatternResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteLogPatternResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLogPattern where
  hashWithSalt _salt DeleteLogPattern' {..} =
    _salt `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` patternSetName
      `Prelude.hashWithSalt` patternName

instance Prelude.NFData DeleteLogPattern where
  rnf DeleteLogPattern' {..} =
    Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf patternSetName
      `Prelude.seq` Prelude.rnf patternName

instance Data.ToHeaders DeleteLogPattern where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "EC2WindowsBarleyService.DeleteLogPattern" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLogPattern where
  toJSON DeleteLogPattern' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResourceGroupName" Data..= resourceGroupName),
            Prelude.Just
              ("PatternSetName" Data..= patternSetName),
            Prelude.Just ("PatternName" Data..= patternName)
          ]
      )

instance Data.ToPath DeleteLogPattern where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLogPattern where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLogPatternResponse' smart constructor.
data DeleteLogPatternResponse = DeleteLogPatternResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLogPatternResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLogPatternResponse_httpStatus' - The response's http status code.
newDeleteLogPatternResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLogPatternResponse
newDeleteLogPatternResponse pHttpStatus_ =
  DeleteLogPatternResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLogPatternResponse_httpStatus :: Lens.Lens' DeleteLogPatternResponse Prelude.Int
deleteLogPatternResponse_httpStatus = Lens.lens (\DeleteLogPatternResponse' {httpStatus} -> httpStatus) (\s@DeleteLogPatternResponse' {} a -> s {httpStatus = a} :: DeleteLogPatternResponse)

instance Prelude.NFData DeleteLogPatternResponse where
  rnf DeleteLogPatternResponse' {..} =
    Prelude.rnf httpStatus

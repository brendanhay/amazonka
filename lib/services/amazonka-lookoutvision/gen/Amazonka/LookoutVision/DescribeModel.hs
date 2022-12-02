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
-- Module      : Amazonka.LookoutVision.DescribeModel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of an Amazon Lookout for Vision model.
--
-- This operation requires permissions to perform the
-- @lookoutvision:DescribeModel@ operation.
module Amazonka.LookoutVision.DescribeModel
  ( -- * Creating a Request
    DescribeModel (..),
    newDescribeModel,

    -- * Request Lenses
    describeModel_projectName,
    describeModel_modelVersion,

    -- * Destructuring the Response
    DescribeModelResponse (..),
    newDescribeModelResponse,

    -- * Response Lenses
    describeModelResponse_modelDescription,
    describeModelResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutVision.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeModel' smart constructor.
data DescribeModel = DescribeModel'
  { -- | The project that contains the version of a model that you want to
    -- describe.
    projectName :: Prelude.Text,
    -- | The version of the model that you want to describe.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'describeModel_projectName' - The project that contains the version of a model that you want to
-- describe.
--
-- 'modelVersion', 'describeModel_modelVersion' - The version of the model that you want to describe.
newDescribeModel ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  DescribeModel
newDescribeModel pProjectName_ pModelVersion_ =
  DescribeModel'
    { projectName = pProjectName_,
      modelVersion = pModelVersion_
    }

-- | The project that contains the version of a model that you want to
-- describe.
describeModel_projectName :: Lens.Lens' DescribeModel Prelude.Text
describeModel_projectName = Lens.lens (\DescribeModel' {projectName} -> projectName) (\s@DescribeModel' {} a -> s {projectName = a} :: DescribeModel)

-- | The version of the model that you want to describe.
describeModel_modelVersion :: Lens.Lens' DescribeModel Prelude.Text
describeModel_modelVersion = Lens.lens (\DescribeModel' {modelVersion} -> modelVersion) (\s@DescribeModel' {} a -> s {modelVersion = a} :: DescribeModel)

instance Core.AWSRequest DescribeModel where
  type
    AWSResponse DescribeModel =
      DescribeModelResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeModelResponse'
            Prelude.<$> (x Data..?> "ModelDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeModel where
  hashWithSalt _salt DescribeModel' {..} =
    _salt `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData DescribeModel where
  rnf DescribeModel' {..} =
    Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf modelVersion

instance Data.ToHeaders DescribeModel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeModel where
  toPath DescribeModel' {..} =
    Prelude.mconcat
      [ "/2020-11-20/projects/",
        Data.toBS projectName,
        "/models/",
        Data.toBS modelVersion
      ]

instance Data.ToQuery DescribeModel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { -- | Contains the description of the model.
    modelDescription :: Prelude.Maybe ModelDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeModelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelDescription', 'describeModelResponse_modelDescription' - Contains the description of the model.
--
-- 'httpStatus', 'describeModelResponse_httpStatus' - The response's http status code.
newDescribeModelResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeModelResponse
newDescribeModelResponse pHttpStatus_ =
  DescribeModelResponse'
    { modelDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the description of the model.
describeModelResponse_modelDescription :: Lens.Lens' DescribeModelResponse (Prelude.Maybe ModelDescription)
describeModelResponse_modelDescription = Lens.lens (\DescribeModelResponse' {modelDescription} -> modelDescription) (\s@DescribeModelResponse' {} a -> s {modelDescription = a} :: DescribeModelResponse)

-- | The response's http status code.
describeModelResponse_httpStatus :: Lens.Lens' DescribeModelResponse Prelude.Int
describeModelResponse_httpStatus = Lens.lens (\DescribeModelResponse' {httpStatus} -> httpStatus) (\s@DescribeModelResponse' {} a -> s {httpStatus = a} :: DescribeModelResponse)

instance Prelude.NFData DescribeModelResponse where
  rnf DescribeModelResponse' {..} =
    Prelude.rnf modelDescription
      `Prelude.seq` Prelude.rnf httpStatus

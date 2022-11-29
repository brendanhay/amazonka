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
-- Module      : Amazonka.Forecast.ResumeResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resumes a stopped monitor resource.
module Amazonka.Forecast.ResumeResource
  ( -- * Creating a Request
    ResumeResource (..),
    newResumeResource,

    -- * Request Lenses
    resumeResource_resourceArn,

    -- * Destructuring the Response
    ResumeResourceResponse (..),
    newResumeResourceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResumeResource' smart constructor.
data ResumeResource = ResumeResource'
  { -- | The Amazon Resource Name (ARN) of the monitor resource to resume.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'resumeResource_resourceArn' - The Amazon Resource Name (ARN) of the monitor resource to resume.
newResumeResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  ResumeResource
newResumeResource pResourceArn_ =
  ResumeResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the monitor resource to resume.
resumeResource_resourceArn :: Lens.Lens' ResumeResource Prelude.Text
resumeResource_resourceArn = Lens.lens (\ResumeResource' {resourceArn} -> resourceArn) (\s@ResumeResource' {} a -> s {resourceArn = a} :: ResumeResource)

instance Core.AWSRequest ResumeResource where
  type
    AWSResponse ResumeResource =
      ResumeResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull ResumeResourceResponse'

instance Prelude.Hashable ResumeResource where
  hashWithSalt _salt ResumeResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ResumeResource where
  rnf ResumeResource' {..} = Prelude.rnf resourceArn

instance Core.ToHeaders ResumeResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.ResumeResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ResumeResource where
  toJSON ResumeResource' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Core..= resourceArn)]
      )

instance Core.ToPath ResumeResource where
  toPath = Prelude.const "/"

instance Core.ToQuery ResumeResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResumeResourceResponse' smart constructor.
data ResumeResourceResponse = ResumeResourceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResumeResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResumeResourceResponse ::
  ResumeResourceResponse
newResumeResourceResponse = ResumeResourceResponse'

instance Prelude.NFData ResumeResourceResponse where
  rnf _ = ()

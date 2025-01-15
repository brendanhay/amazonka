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
-- Module      : Amazonka.ElasticBeanstalk.DescribePlatformVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a platform version. Provides full details. Compare to
-- ListPlatformVersions, which provides summary information about a list of
-- platform versions.
--
-- For definitions of platform version and other platform-related terms,
-- see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/platforms-glossary.html AWS Elastic Beanstalk Platforms Glossary>.
module Amazonka.ElasticBeanstalk.DescribePlatformVersion
  ( -- * Creating a Request
    DescribePlatformVersion (..),
    newDescribePlatformVersion,

    -- * Request Lenses
    describePlatformVersion_platformArn,

    -- * Destructuring the Response
    DescribePlatformVersionResponse (..),
    newDescribePlatformVersionResponse,

    -- * Response Lenses
    describePlatformVersionResponse_platformDescription,
    describePlatformVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePlatformVersion' smart constructor.
data DescribePlatformVersion = DescribePlatformVersion'
  { -- | The ARN of the platform version.
    platformArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlatformVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformArn', 'describePlatformVersion_platformArn' - The ARN of the platform version.
newDescribePlatformVersion ::
  DescribePlatformVersion
newDescribePlatformVersion =
  DescribePlatformVersion'
    { platformArn =
        Prelude.Nothing
    }

-- | The ARN of the platform version.
describePlatformVersion_platformArn :: Lens.Lens' DescribePlatformVersion (Prelude.Maybe Prelude.Text)
describePlatformVersion_platformArn = Lens.lens (\DescribePlatformVersion' {platformArn} -> platformArn) (\s@DescribePlatformVersion' {} a -> s {platformArn = a} :: DescribePlatformVersion)

instance Core.AWSRequest DescribePlatformVersion where
  type
    AWSResponse DescribePlatformVersion =
      DescribePlatformVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribePlatformVersionResult"
      ( \s h x ->
          DescribePlatformVersionResponse'
            Prelude.<$> (x Data..@? "PlatformDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlatformVersion where
  hashWithSalt _salt DescribePlatformVersion' {..} =
    _salt `Prelude.hashWithSalt` platformArn

instance Prelude.NFData DescribePlatformVersion where
  rnf DescribePlatformVersion' {..} =
    Prelude.rnf platformArn

instance Data.ToHeaders DescribePlatformVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribePlatformVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePlatformVersion where
  toQuery DescribePlatformVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribePlatformVersion" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "PlatformArn" Data.=: platformArn
      ]

-- | /See:/ 'newDescribePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { -- | Detailed information about the platform version.
    platformDescription :: Prelude.Maybe PlatformDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePlatformVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformDescription', 'describePlatformVersionResponse_platformDescription' - Detailed information about the platform version.
--
-- 'httpStatus', 'describePlatformVersionResponse_httpStatus' - The response's http status code.
newDescribePlatformVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePlatformVersionResponse
newDescribePlatformVersionResponse pHttpStatus_ =
  DescribePlatformVersionResponse'
    { platformDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the platform version.
describePlatformVersionResponse_platformDescription :: Lens.Lens' DescribePlatformVersionResponse (Prelude.Maybe PlatformDescription)
describePlatformVersionResponse_platformDescription = Lens.lens (\DescribePlatformVersionResponse' {platformDescription} -> platformDescription) (\s@DescribePlatformVersionResponse' {} a -> s {platformDescription = a} :: DescribePlatformVersionResponse)

-- | The response's http status code.
describePlatformVersionResponse_httpStatus :: Lens.Lens' DescribePlatformVersionResponse Prelude.Int
describePlatformVersionResponse_httpStatus = Lens.lens (\DescribePlatformVersionResponse' {httpStatus} -> httpStatus) (\s@DescribePlatformVersionResponse' {} a -> s {httpStatus = a} :: DescribePlatformVersionResponse)

instance
  Prelude.NFData
    DescribePlatformVersionResponse
  where
  rnf DescribePlatformVersionResponse' {..} =
    Prelude.rnf platformDescription `Prelude.seq`
      Prelude.rnf httpStatus

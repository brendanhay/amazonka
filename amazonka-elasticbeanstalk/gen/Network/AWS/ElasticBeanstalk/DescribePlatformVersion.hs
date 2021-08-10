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
-- Module      : Network.AWS.ElasticBeanstalk.DescribePlatformVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.ElasticBeanstalk.DescribePlatformVersion
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

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribePlatformVersionResult"
      ( \s h x ->
          DescribePlatformVersionResponse'
            Prelude.<$> (x Core..@? "PlatformDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePlatformVersion

instance Prelude.NFData DescribePlatformVersion

instance Core.ToHeaders DescribePlatformVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribePlatformVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribePlatformVersion where
  toQuery DescribePlatformVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribePlatformVersion" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "PlatformArn" Core.=: platformArn
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

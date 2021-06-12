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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePlatformVersion' smart constructor.
data DescribePlatformVersion = DescribePlatformVersion'
  { -- | The ARN of the platform version.
    platformArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing
    }

-- | The ARN of the platform version.
describePlatformVersion_platformArn :: Lens.Lens' DescribePlatformVersion (Core.Maybe Core.Text)
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
            Core.<$> (x Core..@? "PlatformDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePlatformVersion

instance Core.NFData DescribePlatformVersion

instance Core.ToHeaders DescribePlatformVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribePlatformVersion where
  toPath = Core.const "/"

instance Core.ToQuery DescribePlatformVersion where
  toQuery DescribePlatformVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribePlatformVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "PlatformArn" Core.=: platformArn
      ]

-- | /See:/ 'newDescribePlatformVersionResponse' smart constructor.
data DescribePlatformVersionResponse = DescribePlatformVersionResponse'
  { -- | Detailed information about the platform version.
    platformDescription :: Core.Maybe PlatformDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribePlatformVersionResponse
newDescribePlatformVersionResponse pHttpStatus_ =
  DescribePlatformVersionResponse'
    { platformDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the platform version.
describePlatformVersionResponse_platformDescription :: Lens.Lens' DescribePlatformVersionResponse (Core.Maybe PlatformDescription)
describePlatformVersionResponse_platformDescription = Lens.lens (\DescribePlatformVersionResponse' {platformDescription} -> platformDescription) (\s@DescribePlatformVersionResponse' {} a -> s {platformDescription = a} :: DescribePlatformVersionResponse)

-- | The response's http status code.
describePlatformVersionResponse_httpStatus :: Lens.Lens' DescribePlatformVersionResponse Core.Int
describePlatformVersionResponse_httpStatus = Lens.lens (\DescribePlatformVersionResponse' {httpStatus} -> httpStatus) (\s@DescribePlatformVersionResponse' {} a -> s {httpStatus = a} :: DescribePlatformVersionResponse)

instance Core.NFData DescribePlatformVersionResponse

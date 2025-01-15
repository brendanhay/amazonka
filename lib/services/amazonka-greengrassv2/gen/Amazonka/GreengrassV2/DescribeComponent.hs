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
-- Module      : Amazonka.GreengrassV2.DescribeComponent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a version of a component.
module Amazonka.GreengrassV2.DescribeComponent
  ( -- * Creating a Request
    DescribeComponent (..),
    newDescribeComponent,

    -- * Request Lenses
    describeComponent_arn,

    -- * Destructuring the Response
    DescribeComponentResponse (..),
    newDescribeComponentResponse,

    -- * Response Lenses
    describeComponentResponse_arn,
    describeComponentResponse_componentName,
    describeComponentResponse_componentVersion,
    describeComponentResponse_creationTimestamp,
    describeComponentResponse_description,
    describeComponentResponse_platforms,
    describeComponentResponse_publisher,
    describeComponentResponse_status,
    describeComponentResponse_tags,
    describeComponentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeComponent' smart constructor.
data DescribeComponent = DescribeComponent'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeComponent_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
newDescribeComponent ::
  -- | 'arn'
  Prelude.Text ->
  DescribeComponent
newDescribeComponent pArn_ =
  DescribeComponent' {arn = pArn_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
describeComponent_arn :: Lens.Lens' DescribeComponent Prelude.Text
describeComponent_arn = Lens.lens (\DescribeComponent' {arn} -> arn) (\s@DescribeComponent' {} a -> s {arn = a} :: DescribeComponent)

instance Core.AWSRequest DescribeComponent where
  type
    AWSResponse DescribeComponent =
      DescribeComponentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeComponentResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "componentName")
            Prelude.<*> (x Data..?> "componentVersion")
            Prelude.<*> (x Data..?> "creationTimestamp")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "platforms" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "publisher")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeComponent where
  hashWithSalt _salt DescribeComponent' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribeComponent where
  rnf DescribeComponent' {..} = Prelude.rnf arn

instance Data.ToHeaders DescribeComponent where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeComponent where
  toPath DescribeComponent' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/components/",
        Data.toBS arn,
        "/metadata"
      ]

instance Data.ToQuery DescribeComponent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeComponentResponse' smart constructor.
data DescribeComponentResponse = DescribeComponentResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the component version.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the component.
    componentName :: Prelude.Maybe Prelude.Text,
    -- | The version of the component.
    componentVersion :: Prelude.Maybe Prelude.Text,
    -- | The time at which the component was created, expressed in ISO 8601
    -- format.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The description of the component version.
    description :: Prelude.Maybe Prelude.Text,
    -- | The platforms that the component version supports.
    platforms :: Prelude.Maybe [ComponentPlatform],
    -- | The publisher of the component version.
    publisher :: Prelude.Maybe Prelude.Text,
    -- | The status of the component version in IoT Greengrass V2. This status is
    -- different from the status of the component on a core device.
    status :: Prelude.Maybe CloudComponentStatus,
    -- | A list of key-value pairs that contain metadata for the resource. For
    -- more information, see
    -- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
    -- in the /IoT Greengrass V2 Developer Guide/.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeComponentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeComponentResponse_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
--
-- 'componentName', 'describeComponentResponse_componentName' - The name of the component.
--
-- 'componentVersion', 'describeComponentResponse_componentVersion' - The version of the component.
--
-- 'creationTimestamp', 'describeComponentResponse_creationTimestamp' - The time at which the component was created, expressed in ISO 8601
-- format.
--
-- 'description', 'describeComponentResponse_description' - The description of the component version.
--
-- 'platforms', 'describeComponentResponse_platforms' - The platforms that the component version supports.
--
-- 'publisher', 'describeComponentResponse_publisher' - The publisher of the component version.
--
-- 'status', 'describeComponentResponse_status' - The status of the component version in IoT Greengrass V2. This status is
-- different from the status of the component on a core device.
--
-- 'tags', 'describeComponentResponse_tags' - A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- 'httpStatus', 'describeComponentResponse_httpStatus' - The response's http status code.
newDescribeComponentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeComponentResponse
newDescribeComponentResponse pHttpStatus_ =
  DescribeComponentResponse'
    { arn = Prelude.Nothing,
      componentName = Prelude.Nothing,
      componentVersion = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      description = Prelude.Nothing,
      platforms = Prelude.Nothing,
      publisher = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the component version.
describeComponentResponse_arn :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.Text)
describeComponentResponse_arn = Lens.lens (\DescribeComponentResponse' {arn} -> arn) (\s@DescribeComponentResponse' {} a -> s {arn = a} :: DescribeComponentResponse)

-- | The name of the component.
describeComponentResponse_componentName :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.Text)
describeComponentResponse_componentName = Lens.lens (\DescribeComponentResponse' {componentName} -> componentName) (\s@DescribeComponentResponse' {} a -> s {componentName = a} :: DescribeComponentResponse)

-- | The version of the component.
describeComponentResponse_componentVersion :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.Text)
describeComponentResponse_componentVersion = Lens.lens (\DescribeComponentResponse' {componentVersion} -> componentVersion) (\s@DescribeComponentResponse' {} a -> s {componentVersion = a} :: DescribeComponentResponse)

-- | The time at which the component was created, expressed in ISO 8601
-- format.
describeComponentResponse_creationTimestamp :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.UTCTime)
describeComponentResponse_creationTimestamp = Lens.lens (\DescribeComponentResponse' {creationTimestamp} -> creationTimestamp) (\s@DescribeComponentResponse' {} a -> s {creationTimestamp = a} :: DescribeComponentResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the component version.
describeComponentResponse_description :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.Text)
describeComponentResponse_description = Lens.lens (\DescribeComponentResponse' {description} -> description) (\s@DescribeComponentResponse' {} a -> s {description = a} :: DescribeComponentResponse)

-- | The platforms that the component version supports.
describeComponentResponse_platforms :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe [ComponentPlatform])
describeComponentResponse_platforms = Lens.lens (\DescribeComponentResponse' {platforms} -> platforms) (\s@DescribeComponentResponse' {} a -> s {platforms = a} :: DescribeComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The publisher of the component version.
describeComponentResponse_publisher :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe Prelude.Text)
describeComponentResponse_publisher = Lens.lens (\DescribeComponentResponse' {publisher} -> publisher) (\s@DescribeComponentResponse' {} a -> s {publisher = a} :: DescribeComponentResponse)

-- | The status of the component version in IoT Greengrass V2. This status is
-- different from the status of the component on a core device.
describeComponentResponse_status :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe CloudComponentStatus)
describeComponentResponse_status = Lens.lens (\DescribeComponentResponse' {status} -> status) (\s@DescribeComponentResponse' {} a -> s {status = a} :: DescribeComponentResponse)

-- | A list of key-value pairs that contain metadata for the resource. For
-- more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/tag-resources.html Tag your resources>
-- in the /IoT Greengrass V2 Developer Guide/.
describeComponentResponse_tags :: Lens.Lens' DescribeComponentResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeComponentResponse_tags = Lens.lens (\DescribeComponentResponse' {tags} -> tags) (\s@DescribeComponentResponse' {} a -> s {tags = a} :: DescribeComponentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeComponentResponse_httpStatus :: Lens.Lens' DescribeComponentResponse Prelude.Int
describeComponentResponse_httpStatus = Lens.lens (\DescribeComponentResponse' {httpStatus} -> httpStatus) (\s@DescribeComponentResponse' {} a -> s {httpStatus = a} :: DescribeComponentResponse)

instance Prelude.NFData DescribeComponentResponse where
  rnf DescribeComponentResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf componentName `Prelude.seq`
        Prelude.rnf componentVersion `Prelude.seq`
          Prelude.rnf creationTimestamp `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf platforms `Prelude.seq`
                Prelude.rnf publisher `Prelude.seq`
                  Prelude.rnf status `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf httpStatus

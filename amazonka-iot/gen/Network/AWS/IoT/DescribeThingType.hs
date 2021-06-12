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
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
module Network.AWS.IoT.DescribeThingType
  ( -- * Creating a Request
    DescribeThingType (..),
    newDescribeThingType,

    -- * Request Lenses
    describeThingType_thingTypeName,

    -- * Destructuring the Response
    DescribeThingTypeResponse (..),
    newDescribeThingTypeResponse,

    -- * Response Lenses
    describeThingTypeResponse_thingTypeProperties,
    describeThingTypeResponse_thingTypeMetadata,
    describeThingTypeResponse_thingTypeId,
    describeThingTypeResponse_thingTypeArn,
    describeThingTypeResponse_thingTypeName,
    describeThingTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeThingType operation.
--
-- /See:/ 'newDescribeThingType' smart constructor.
data DescribeThingType = DescribeThingType'
  { -- | The name of the thing type.
    thingTypeName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeThingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeName', 'describeThingType_thingTypeName' - The name of the thing type.
newDescribeThingType ::
  -- | 'thingTypeName'
  Core.Text ->
  DescribeThingType
newDescribeThingType pThingTypeName_ =
  DescribeThingType' {thingTypeName = pThingTypeName_}

-- | The name of the thing type.
describeThingType_thingTypeName :: Lens.Lens' DescribeThingType Core.Text
describeThingType_thingTypeName = Lens.lens (\DescribeThingType' {thingTypeName} -> thingTypeName) (\s@DescribeThingType' {} a -> s {thingTypeName = a} :: DescribeThingType)

instance Core.AWSRequest DescribeThingType where
  type
    AWSResponse DescribeThingType =
      DescribeThingTypeResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingTypeResponse'
            Core.<$> (x Core..?> "thingTypeProperties")
            Core.<*> (x Core..?> "thingTypeMetadata")
            Core.<*> (x Core..?> "thingTypeId")
            Core.<*> (x Core..?> "thingTypeArn")
            Core.<*> (x Core..?> "thingTypeName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeThingType

instance Core.NFData DescribeThingType

instance Core.ToHeaders DescribeThingType where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeThingType where
  toPath DescribeThingType' {..} =
    Core.mconcat
      ["/thing-types/", Core.toBS thingTypeName]

instance Core.ToQuery DescribeThingType where
  toQuery = Core.const Core.mempty

-- | The output for the DescribeThingType operation.
--
-- /See:/ 'newDescribeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { -- | The ThingTypeProperties contains information about the thing type
    -- including description, and a list of searchable thing attribute names.
    thingTypeProperties :: Core.Maybe ThingTypeProperties,
    -- | The ThingTypeMetadata contains additional information about the thing
    -- type including: creation date and time, a value indicating whether the
    -- thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Core.Maybe ThingTypeMetadata,
    -- | The thing type ID.
    thingTypeId :: Core.Maybe Core.Text,
    -- | The thing type ARN.
    thingTypeArn :: Core.Maybe Core.Text,
    -- | The name of the thing type.
    thingTypeName :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeThingTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeProperties', 'describeThingTypeResponse_thingTypeProperties' - The ThingTypeProperties contains information about the thing type
-- including description, and a list of searchable thing attribute names.
--
-- 'thingTypeMetadata', 'describeThingTypeResponse_thingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
--
-- 'thingTypeId', 'describeThingTypeResponse_thingTypeId' - The thing type ID.
--
-- 'thingTypeArn', 'describeThingTypeResponse_thingTypeArn' - The thing type ARN.
--
-- 'thingTypeName', 'describeThingTypeResponse_thingTypeName' - The name of the thing type.
--
-- 'httpStatus', 'describeThingTypeResponse_httpStatus' - The response's http status code.
newDescribeThingTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeThingTypeResponse
newDescribeThingTypeResponse pHttpStatus_ =
  DescribeThingTypeResponse'
    { thingTypeProperties =
        Core.Nothing,
      thingTypeMetadata = Core.Nothing,
      thingTypeId = Core.Nothing,
      thingTypeArn = Core.Nothing,
      thingTypeName = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ThingTypeProperties contains information about the thing type
-- including description, and a list of searchable thing attribute names.
describeThingTypeResponse_thingTypeProperties :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe ThingTypeProperties)
describeThingTypeResponse_thingTypeProperties = Lens.lens (\DescribeThingTypeResponse' {thingTypeProperties} -> thingTypeProperties) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeProperties = a} :: DescribeThingTypeResponse)

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
describeThingTypeResponse_thingTypeMetadata :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe ThingTypeMetadata)
describeThingTypeResponse_thingTypeMetadata = Lens.lens (\DescribeThingTypeResponse' {thingTypeMetadata} -> thingTypeMetadata) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeMetadata = a} :: DescribeThingTypeResponse)

-- | The thing type ID.
describeThingTypeResponse_thingTypeId :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Core.Text)
describeThingTypeResponse_thingTypeId = Lens.lens (\DescribeThingTypeResponse' {thingTypeId} -> thingTypeId) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeId = a} :: DescribeThingTypeResponse)

-- | The thing type ARN.
describeThingTypeResponse_thingTypeArn :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Core.Text)
describeThingTypeResponse_thingTypeArn = Lens.lens (\DescribeThingTypeResponse' {thingTypeArn} -> thingTypeArn) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeArn = a} :: DescribeThingTypeResponse)

-- | The name of the thing type.
describeThingTypeResponse_thingTypeName :: Lens.Lens' DescribeThingTypeResponse (Core.Maybe Core.Text)
describeThingTypeResponse_thingTypeName = Lens.lens (\DescribeThingTypeResponse' {thingTypeName} -> thingTypeName) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeName = a} :: DescribeThingTypeResponse)

-- | The response's http status code.
describeThingTypeResponse_httpStatus :: Lens.Lens' DescribeThingTypeResponse Core.Int
describeThingTypeResponse_httpStatus = Lens.lens (\DescribeThingTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeThingTypeResponse' {} a -> s {httpStatus = a} :: DescribeThingTypeResponse)

instance Core.NFData DescribeThingTypeResponse

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeThingType operation.
--
-- /See:/ 'newDescribeThingType' smart constructor.
data DescribeThingType = DescribeThingType'
  { -- | The name of the thing type.
    thingTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeThingType
newDescribeThingType pThingTypeName_ =
  DescribeThingType' {thingTypeName = pThingTypeName_}

-- | The name of the thing type.
describeThingType_thingTypeName :: Lens.Lens' DescribeThingType Prelude.Text
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
            Prelude.<$> (x Core..?> "thingTypeProperties")
            Prelude.<*> (x Core..?> "thingTypeMetadata")
            Prelude.<*> (x Core..?> "thingTypeId")
            Prelude.<*> (x Core..?> "thingTypeArn")
            Prelude.<*> (x Core..?> "thingTypeName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeThingType

instance Prelude.NFData DescribeThingType

instance Core.ToHeaders DescribeThingType where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeThingType where
  toPath DescribeThingType' {..} =
    Prelude.mconcat
      ["/thing-types/", Core.toBS thingTypeName]

instance Core.ToQuery DescribeThingType where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DescribeThingType operation.
--
-- /See:/ 'newDescribeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { -- | The ThingTypeProperties contains information about the thing type
    -- including description, and a list of searchable thing attribute names.
    thingTypeProperties :: Prelude.Maybe ThingTypeProperties,
    -- | The ThingTypeMetadata contains additional information about the thing
    -- type including: creation date and time, a value indicating whether the
    -- thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Prelude.Maybe ThingTypeMetadata,
    -- | The thing type ID.
    thingTypeId :: Prelude.Maybe Prelude.Text,
    -- | The thing type ARN.
    thingTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeThingTypeResponse
newDescribeThingTypeResponse pHttpStatus_ =
  DescribeThingTypeResponse'
    { thingTypeProperties =
        Prelude.Nothing,
      thingTypeMetadata = Prelude.Nothing,
      thingTypeId = Prelude.Nothing,
      thingTypeArn = Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ThingTypeProperties contains information about the thing type
-- including description, and a list of searchable thing attribute names.
describeThingTypeResponse_thingTypeProperties :: Lens.Lens' DescribeThingTypeResponse (Prelude.Maybe ThingTypeProperties)
describeThingTypeResponse_thingTypeProperties = Lens.lens (\DescribeThingTypeResponse' {thingTypeProperties} -> thingTypeProperties) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeProperties = a} :: DescribeThingTypeResponse)

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
describeThingTypeResponse_thingTypeMetadata :: Lens.Lens' DescribeThingTypeResponse (Prelude.Maybe ThingTypeMetadata)
describeThingTypeResponse_thingTypeMetadata = Lens.lens (\DescribeThingTypeResponse' {thingTypeMetadata} -> thingTypeMetadata) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeMetadata = a} :: DescribeThingTypeResponse)

-- | The thing type ID.
describeThingTypeResponse_thingTypeId :: Lens.Lens' DescribeThingTypeResponse (Prelude.Maybe Prelude.Text)
describeThingTypeResponse_thingTypeId = Lens.lens (\DescribeThingTypeResponse' {thingTypeId} -> thingTypeId) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeId = a} :: DescribeThingTypeResponse)

-- | The thing type ARN.
describeThingTypeResponse_thingTypeArn :: Lens.Lens' DescribeThingTypeResponse (Prelude.Maybe Prelude.Text)
describeThingTypeResponse_thingTypeArn = Lens.lens (\DescribeThingTypeResponse' {thingTypeArn} -> thingTypeArn) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeArn = a} :: DescribeThingTypeResponse)

-- | The name of the thing type.
describeThingTypeResponse_thingTypeName :: Lens.Lens' DescribeThingTypeResponse (Prelude.Maybe Prelude.Text)
describeThingTypeResponse_thingTypeName = Lens.lens (\DescribeThingTypeResponse' {thingTypeName} -> thingTypeName) (\s@DescribeThingTypeResponse' {} a -> s {thingTypeName = a} :: DescribeThingTypeResponse)

-- | The response's http status code.
describeThingTypeResponse_httpStatus :: Lens.Lens' DescribeThingTypeResponse Prelude.Int
describeThingTypeResponse_httpStatus = Lens.lens (\DescribeThingTypeResponse' {httpStatus} -> httpStatus) (\s@DescribeThingTypeResponse' {} a -> s {httpStatus = a} :: DescribeThingTypeResponse)

instance Prelude.NFData DescribeThingTypeResponse

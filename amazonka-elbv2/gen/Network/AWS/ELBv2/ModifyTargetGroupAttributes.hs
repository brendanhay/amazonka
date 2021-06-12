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
-- Module      : Network.AWS.ELBv2.ModifyTargetGroupAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified target group.
module Network.AWS.ELBv2.ModifyTargetGroupAttributes
  ( -- * Creating a Request
    ModifyTargetGroupAttributes (..),
    newModifyTargetGroupAttributes,

    -- * Request Lenses
    modifyTargetGroupAttributes_targetGroupArn,
    modifyTargetGroupAttributes_attributes,

    -- * Destructuring the Response
    ModifyTargetGroupAttributesResponse (..),
    newModifyTargetGroupAttributesResponse,

    -- * Response Lenses
    modifyTargetGroupAttributesResponse_attributes,
    modifyTargetGroupAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTargetGroupAttributes' smart constructor.
data ModifyTargetGroupAttributes = ModifyTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Text,
    -- | The attributes.
    attributes :: [TargetGroupAttribute]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTargetGroupAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'modifyTargetGroupAttributes_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'attributes', 'modifyTargetGroupAttributes_attributes' - The attributes.
newModifyTargetGroupAttributes ::
  -- | 'targetGroupArn'
  Core.Text ->
  ModifyTargetGroupAttributes
newModifyTargetGroupAttributes pTargetGroupArn_ =
  ModifyTargetGroupAttributes'
    { targetGroupArn =
        pTargetGroupArn_,
      attributes = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
modifyTargetGroupAttributes_targetGroupArn :: Lens.Lens' ModifyTargetGroupAttributes Core.Text
modifyTargetGroupAttributes_targetGroupArn = Lens.lens (\ModifyTargetGroupAttributes' {targetGroupArn} -> targetGroupArn) (\s@ModifyTargetGroupAttributes' {} a -> s {targetGroupArn = a} :: ModifyTargetGroupAttributes)

-- | The attributes.
modifyTargetGroupAttributes_attributes :: Lens.Lens' ModifyTargetGroupAttributes [TargetGroupAttribute]
modifyTargetGroupAttributes_attributes = Lens.lens (\ModifyTargetGroupAttributes' {attributes} -> attributes) (\s@ModifyTargetGroupAttributes' {} a -> s {attributes = a} :: ModifyTargetGroupAttributes) Core.. Lens._Coerce

instance Core.AWSRequest ModifyTargetGroupAttributes where
  type
    AWSResponse ModifyTargetGroupAttributes =
      ModifyTargetGroupAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupAttributesResult"
      ( \s h x ->
          ModifyTargetGroupAttributesResponse'
            Core.<$> ( x Core..@? "Attributes" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyTargetGroupAttributes

instance Core.NFData ModifyTargetGroupAttributes

instance Core.ToHeaders ModifyTargetGroupAttributes where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyTargetGroupAttributes where
  toPath = Core.const "/"

instance Core.ToQuery ModifyTargetGroupAttributes where
  toQuery ModifyTargetGroupAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyTargetGroupAttributes" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "TargetGroupArn" Core.=: targetGroupArn,
        "Attributes"
          Core.=: Core.toQueryList "member" attributes
      ]

-- | /See:/ 'newModifyTargetGroupAttributesResponse' smart constructor.
data ModifyTargetGroupAttributesResponse = ModifyTargetGroupAttributesResponse'
  { -- | Information about the attributes.
    attributes :: Core.Maybe [TargetGroupAttribute],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTargetGroupAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'modifyTargetGroupAttributesResponse_attributes' - Information about the attributes.
--
-- 'httpStatus', 'modifyTargetGroupAttributesResponse_httpStatus' - The response's http status code.
newModifyTargetGroupAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyTargetGroupAttributesResponse
newModifyTargetGroupAttributesResponse pHttpStatus_ =
  ModifyTargetGroupAttributesResponse'
    { attributes =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attributes.
modifyTargetGroupAttributesResponse_attributes :: Lens.Lens' ModifyTargetGroupAttributesResponse (Core.Maybe [TargetGroupAttribute])
modifyTargetGroupAttributesResponse_attributes = Lens.lens (\ModifyTargetGroupAttributesResponse' {attributes} -> attributes) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {attributes = a} :: ModifyTargetGroupAttributesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyTargetGroupAttributesResponse_httpStatus :: Lens.Lens' ModifyTargetGroupAttributesResponse Core.Int
modifyTargetGroupAttributesResponse_httpStatus = Lens.lens (\ModifyTargetGroupAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {httpStatus = a} :: ModifyTargetGroupAttributesResponse)

instance
  Core.NFData
    ModifyTargetGroupAttributesResponse

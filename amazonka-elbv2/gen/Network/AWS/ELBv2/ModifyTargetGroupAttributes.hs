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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTargetGroupAttributes' smart constructor.
data ModifyTargetGroupAttributes = ModifyTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Text,
    -- | The attributes.
    attributes :: [TargetGroupAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ModifyTargetGroupAttributes
newModifyTargetGroupAttributes pTargetGroupArn_ =
  ModifyTargetGroupAttributes'
    { targetGroupArn =
        pTargetGroupArn_,
      attributes = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
modifyTargetGroupAttributes_targetGroupArn :: Lens.Lens' ModifyTargetGroupAttributes Prelude.Text
modifyTargetGroupAttributes_targetGroupArn = Lens.lens (\ModifyTargetGroupAttributes' {targetGroupArn} -> targetGroupArn) (\s@ModifyTargetGroupAttributes' {} a -> s {targetGroupArn = a} :: ModifyTargetGroupAttributes)

-- | The attributes.
modifyTargetGroupAttributes_attributes :: Lens.Lens' ModifyTargetGroupAttributes [TargetGroupAttribute]
modifyTargetGroupAttributes_attributes = Lens.lens (\ModifyTargetGroupAttributes' {attributes} -> attributes) (\s@ModifyTargetGroupAttributes' {} a -> s {attributes = a} :: ModifyTargetGroupAttributes) Prelude.. Lens._Coerce

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
            Prelude.<$> ( x Core..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyTargetGroupAttributes

instance Prelude.NFData ModifyTargetGroupAttributes

instance Core.ToHeaders ModifyTargetGroupAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyTargetGroupAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyTargetGroupAttributes where
  toQuery ModifyTargetGroupAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "ModifyTargetGroupAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Core.=: targetGroupArn,
        "Attributes"
          Core.=: Core.toQueryList "member" attributes
      ]

-- | /See:/ 'newModifyTargetGroupAttributesResponse' smart constructor.
data ModifyTargetGroupAttributesResponse = ModifyTargetGroupAttributesResponse'
  { -- | Information about the attributes.
    attributes :: Prelude.Maybe [TargetGroupAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ModifyTargetGroupAttributesResponse
newModifyTargetGroupAttributesResponse pHttpStatus_ =
  ModifyTargetGroupAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the attributes.
modifyTargetGroupAttributesResponse_attributes :: Lens.Lens' ModifyTargetGroupAttributesResponse (Prelude.Maybe [TargetGroupAttribute])
modifyTargetGroupAttributesResponse_attributes = Lens.lens (\ModifyTargetGroupAttributesResponse' {attributes} -> attributes) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {attributes = a} :: ModifyTargetGroupAttributesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
modifyTargetGroupAttributesResponse_httpStatus :: Lens.Lens' ModifyTargetGroupAttributesResponse Prelude.Int
modifyTargetGroupAttributesResponse_httpStatus = Lens.lens (\ModifyTargetGroupAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {httpStatus = a} :: ModifyTargetGroupAttributesResponse)

instance
  Prelude.NFData
    ModifyTargetGroupAttributesResponse

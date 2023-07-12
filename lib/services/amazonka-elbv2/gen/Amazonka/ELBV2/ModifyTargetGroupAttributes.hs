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
-- Module      : Amazonka.ELBV2.ModifyTargetGroupAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified target group.
module Amazonka.ELBV2.ModifyTargetGroupAttributes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
modifyTargetGroupAttributes_attributes = Lens.lens (\ModifyTargetGroupAttributes' {attributes} -> attributes) (\s@ModifyTargetGroupAttributes' {} a -> s {attributes = a} :: ModifyTargetGroupAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyTargetGroupAttributes where
  type
    AWSResponse ModifyTargetGroupAttributes =
      ModifyTargetGroupAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyTargetGroupAttributesResult"
      ( \s h x ->
          ModifyTargetGroupAttributesResponse'
            Prelude.<$> ( x
                            Data..@? "Attributes"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyTargetGroupAttributes where
  hashWithSalt _salt ModifyTargetGroupAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData ModifyTargetGroupAttributes where
  rnf ModifyTargetGroupAttributes' {..} =
    Prelude.rnf targetGroupArn
      `Prelude.seq` Prelude.rnf attributes

instance Data.ToHeaders ModifyTargetGroupAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyTargetGroupAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyTargetGroupAttributes where
  toQuery ModifyTargetGroupAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyTargetGroupAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "TargetGroupArn" Data.=: targetGroupArn,
        "Attributes"
          Data.=: Data.toQueryList "member" attributes
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
modifyTargetGroupAttributesResponse_attributes = Lens.lens (\ModifyTargetGroupAttributesResponse' {attributes} -> attributes) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {attributes = a} :: ModifyTargetGroupAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyTargetGroupAttributesResponse_httpStatus :: Lens.Lens' ModifyTargetGroupAttributesResponse Prelude.Int
modifyTargetGroupAttributesResponse_httpStatus = Lens.lens (\ModifyTargetGroupAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyTargetGroupAttributesResponse' {} a -> s {httpStatus = a} :: ModifyTargetGroupAttributesResponse)

instance
  Prelude.NFData
    ModifyTargetGroupAttributesResponse
  where
  rnf ModifyTargetGroupAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus

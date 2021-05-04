{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.DetachTypedLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a typed link from a specified source and target object. For
-- more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Network.AWS.CloudDirectory.DetachTypedLink
  ( -- * Creating a Request
    DetachTypedLink (..),
    newDetachTypedLink,

    -- * Request Lenses
    detachTypedLink_directoryArn,
    detachTypedLink_typedLinkSpecifier,

    -- * Destructuring the Response
    DetachTypedLinkResponse (..),
    newDetachTypedLinkResponse,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachTypedLink' smart constructor.
data DetachTypedLink = DetachTypedLink'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to detach
    -- the typed link.
    directoryArn :: Prelude.Text,
    -- | Used to accept a typed link specifier as input.
    typedLinkSpecifier :: TypedLinkSpecifier
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachTypedLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'detachTypedLink_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to detach
-- the typed link.
--
-- 'typedLinkSpecifier', 'detachTypedLink_typedLinkSpecifier' - Used to accept a typed link specifier as input.
newDetachTypedLink ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  DetachTypedLink
newDetachTypedLink
  pDirectoryArn_
  pTypedLinkSpecifier_ =
    DetachTypedLink'
      { directoryArn = pDirectoryArn_,
        typedLinkSpecifier = pTypedLinkSpecifier_
      }

-- | The Amazon Resource Name (ARN) of the directory where you want to detach
-- the typed link.
detachTypedLink_directoryArn :: Lens.Lens' DetachTypedLink Prelude.Text
detachTypedLink_directoryArn = Lens.lens (\DetachTypedLink' {directoryArn} -> directoryArn) (\s@DetachTypedLink' {} a -> s {directoryArn = a} :: DetachTypedLink)

-- | Used to accept a typed link specifier as input.
detachTypedLink_typedLinkSpecifier :: Lens.Lens' DetachTypedLink TypedLinkSpecifier
detachTypedLink_typedLinkSpecifier = Lens.lens (\DetachTypedLink' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@DetachTypedLink' {} a -> s {typedLinkSpecifier = a} :: DetachTypedLink)

instance Prelude.AWSRequest DetachTypedLink where
  type Rs DetachTypedLink = DetachTypedLinkResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull DetachTypedLinkResponse'

instance Prelude.Hashable DetachTypedLink

instance Prelude.NFData DetachTypedLink

instance Prelude.ToHeaders DetachTypedLink where
  toHeaders DetachTypedLink' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON DetachTypedLink where
  toJSON DetachTypedLink' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "TypedLinkSpecifier"
                  Prelude..= typedLinkSpecifier
              )
          ]
      )

instance Prelude.ToPath DetachTypedLink where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/detach"

instance Prelude.ToQuery DetachTypedLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachTypedLinkResponse' smart constructor.
data DetachTypedLinkResponse = DetachTypedLinkResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachTypedLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachTypedLinkResponse ::
  DetachTypedLinkResponse
newDetachTypedLinkResponse = DetachTypedLinkResponse'

instance Prelude.NFData DetachTypedLinkResponse

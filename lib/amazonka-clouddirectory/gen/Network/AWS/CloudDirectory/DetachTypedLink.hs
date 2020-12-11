{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a typed link from a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.DetachTypedLink
  ( -- * Creating a request
    DetachTypedLink (..),
    mkDetachTypedLink,

    -- ** Request lenses
    dtlDirectoryARN,
    dtlTypedLinkSpecifier,

    -- * Destructuring the response
    DetachTypedLinkResponse (..),
    mkDetachTypedLinkResponse,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachTypedLink' smart constructor.
data DetachTypedLink = DetachTypedLink'
  { directoryARN :: Lude.Text,
    typedLinkSpecifier :: TypedLinkSpecifier
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachTypedLink' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
-- * 'typedLinkSpecifier' - Used to accept a typed link specifier as input.
mkDetachTypedLink ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  DetachTypedLink
mkDetachTypedLink pDirectoryARN_ pTypedLinkSpecifier_ =
  DetachTypedLink'
    { directoryARN = pDirectoryARN_,
      typedLinkSpecifier = pTypedLinkSpecifier_
    }

-- | The Amazon Resource Name (ARN) of the directory where you want to detach the typed link.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlDirectoryARN :: Lens.Lens' DetachTypedLink Lude.Text
dtlDirectoryARN = Lens.lens (directoryARN :: DetachTypedLink -> Lude.Text) (\s a -> s {directoryARN = a} :: DetachTypedLink)
{-# DEPRECATED dtlDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Used to accept a typed link specifier as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtlTypedLinkSpecifier :: Lens.Lens' DetachTypedLink TypedLinkSpecifier
dtlTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: DetachTypedLink -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: DetachTypedLink)
{-# DEPRECATED dtlTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

instance Lude.AWSRequest DetachTypedLink where
  type Rs DetachTypedLink = DetachTypedLinkResponse
  request = Req.putJSON cloudDirectoryService
  response = Res.receiveNull DetachTypedLinkResponse'

instance Lude.ToHeaders DetachTypedLink where
  toHeaders DetachTypedLink' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DetachTypedLink where
  toJSON DetachTypedLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier)]
      )

instance Lude.ToPath DetachTypedLink where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/detach"

instance Lude.ToQuery DetachTypedLink where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachTypedLinkResponse' smart constructor.
data DetachTypedLinkResponse = DetachTypedLinkResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachTypedLinkResponse' with the minimum fields required to make a request.
mkDetachTypedLinkResponse ::
  DetachTypedLinkResponse
mkDetachTypedLinkResponse = DetachTypedLinkResponse'

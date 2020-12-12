{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.ObjectReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.ObjectReference
  ( ObjectReference (..),

    -- * Smart constructor
    mkObjectReference,

    -- * Lenses
    orSelector,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reference that identifies an object.
--
-- /See:/ 'mkObjectReference' smart constructor.
newtype ObjectReference = ObjectReference'
  { selector ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectReference' with the minimum fields required to make a request.
--
-- * 'selector' - A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects> . You can identify an object in one of the following ways:
--
--
--     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier
--
--
--     * /\/some\/path/ - Identifies the object based on path
--
--
--     * /#SomeBatchReference/ - Identifies the object in a batch call
mkObjectReference ::
  ObjectReference
mkObjectReference = ObjectReference' {selector = Lude.Nothing}

-- | A path selector supports easy selection of an object by the parent/child links leading to it from the directory root. Use the link names from each parent/child link to construct the path. Path selectors start with a slash (/) and link names are separated by slashes. For more information about paths, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_access_objects.html Access Objects> . You can identify an object in one of the following ways:
--
--
--     * /> ObjectIdentifier/ - An object identifier is an opaque string provided by Amazon Cloud Directory. When creating objects, the system will provide you with the identifier of the created object. An object’s identifier is immutable and no two objects will ever share the same object identifier
--
--
--     * /\/some\/path/ - Identifies the object based on path
--
--
--     * /#SomeBatchReference/ - Identifies the object in a batch call
--
--
--
-- /Note:/ Consider using 'selector' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
orSelector :: Lens.Lens' ObjectReference (Lude.Maybe Lude.Text)
orSelector = Lens.lens (selector :: ObjectReference -> Lude.Maybe Lude.Text) (\s a -> s {selector = a} :: ObjectReference)
{-# DEPRECATED orSelector "Use generic-lens or generic-optics with 'selector' instead." #-}

instance Lude.FromJSON ObjectReference where
  parseJSON =
    Lude.withObject
      "ObjectReference"
      (\x -> ObjectReference' Lude.<$> (x Lude..:? "Selector"))

instance Lude.ToJSON ObjectReference where
  toJSON ObjectReference' {..} =
    Lude.object
      (Lude.catMaybes [("Selector" Lude..=) Lude.<$> selector])

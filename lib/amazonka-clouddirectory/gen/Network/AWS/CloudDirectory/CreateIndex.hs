{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.CreateIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an index object. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/indexing_search.html Indexing and search> for more information.
module Network.AWS.CloudDirectory.CreateIndex
  ( -- * Creating a request
    CreateIndex (..),
    mkCreateIndex,

    -- ** Request lenses
    ciParentReference,
    ciLinkName,
    ciDirectoryARN,
    ciOrderedIndexedAttributeList,
    ciIsUnique,

    -- * Destructuring the response
    CreateIndexResponse (..),
    mkCreateIndexResponse,

    -- ** Response lenses
    cirsObjectIdentifier,
    cirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateIndex' smart constructor.
data CreateIndex = CreateIndex'
  { parentReference ::
      Lude.Maybe ObjectReference,
    linkName :: Lude.Maybe Lude.Text,
    directoryARN :: Lude.Text,
    orderedIndexedAttributeList :: [AttributeKey],
    isUnique :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIndex' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory where the index should be created.
-- * 'isUnique' - Indicates whether the attribute that is being indexed has unique values or not.
-- * 'linkName' - The name of the link between the parent object and the index object.
-- * 'orderedIndexedAttributeList' - Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
-- * 'parentReference' - A reference to the parent object that contains the index object.
mkCreateIndex ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'isUnique'
  Lude.Bool ->
  CreateIndex
mkCreateIndex pDirectoryARN_ pIsUnique_ =
  CreateIndex'
    { parentReference = Lude.Nothing,
      linkName = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      orderedIndexedAttributeList = Lude.mempty,
      isUnique = pIsUnique_
    }

-- | A reference to the parent object that contains the index object.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciParentReference :: Lens.Lens' CreateIndex (Lude.Maybe ObjectReference)
ciParentReference = Lens.lens (parentReference :: CreateIndex -> Lude.Maybe ObjectReference) (\s a -> s {parentReference = a} :: CreateIndex)
{-# DEPRECATED ciParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The name of the link between the parent object and the index object.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciLinkName :: Lens.Lens' CreateIndex (Lude.Maybe Lude.Text)
ciLinkName = Lens.lens (linkName :: CreateIndex -> Lude.Maybe Lude.Text) (\s a -> s {linkName = a} :: CreateIndex)
{-# DEPRECATED ciLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

-- | The ARN of the directory where the index should be created.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciDirectoryARN :: Lens.Lens' CreateIndex Lude.Text
ciDirectoryARN = Lens.lens (directoryARN :: CreateIndex -> Lude.Text) (\s a -> s {directoryARN = a} :: CreateIndex)
{-# DEPRECATED ciDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Specifies the attributes that should be indexed on. Currently only a single attribute is supported.
--
-- /Note:/ Consider using 'orderedIndexedAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciOrderedIndexedAttributeList :: Lens.Lens' CreateIndex [AttributeKey]
ciOrderedIndexedAttributeList = Lens.lens (orderedIndexedAttributeList :: CreateIndex -> [AttributeKey]) (\s a -> s {orderedIndexedAttributeList = a} :: CreateIndex)
{-# DEPRECATED ciOrderedIndexedAttributeList "Use generic-lens or generic-optics with 'orderedIndexedAttributeList' instead." #-}

-- | Indicates whether the attribute that is being indexed has unique values or not.
--
-- /Note:/ Consider using 'isUnique' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciIsUnique :: Lens.Lens' CreateIndex Lude.Bool
ciIsUnique = Lens.lens (isUnique :: CreateIndex -> Lude.Bool) (\s a -> s {isUnique = a} :: CreateIndex)
{-# DEPRECATED ciIsUnique "Use generic-lens or generic-optics with 'isUnique' instead." #-}

instance Lude.AWSRequest CreateIndex where
  type Rs CreateIndex = CreateIndexResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateIndexResponse'
            Lude.<$> (x Lude..?> "ObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateIndex where
  toHeaders CreateIndex' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON CreateIndex where
  toJSON CreateIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentReference" Lude..=) Lude.<$> parentReference,
            ("LinkName" Lude..=) Lude.<$> linkName,
            Lude.Just
              ( "OrderedIndexedAttributeList"
                  Lude..= orderedIndexedAttributeList
              ),
            Lude.Just ("IsUnique" Lude..= isUnique)
          ]
      )

instance Lude.ToPath CreateIndex where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/index"

instance Lude.ToQuery CreateIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateIndexResponse' smart constructor.
data CreateIndexResponse = CreateIndexResponse'
  { objectIdentifier ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIndexResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ of the index created by this operation.
-- * 'responseStatus' - The response status code.
mkCreateIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateIndexResponse
mkCreateIndexResponse pResponseStatus_ =
  CreateIndexResponse'
    { objectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ of the index created by this operation.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsObjectIdentifier :: Lens.Lens' CreateIndexResponse (Lude.Maybe Lude.Text)
cirsObjectIdentifier = Lens.lens (objectIdentifier :: CreateIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: CreateIndexResponse)
{-# DEPRECATED cirsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateIndexResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateIndexResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

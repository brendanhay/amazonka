{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListInventoryEntries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of inventory items returned by the request.
module Network.AWS.SSM.ListInventoryEntries
  ( -- * Creating a request
    ListInventoryEntries (..),
    mkListInventoryEntries,

    -- ** Request lenses
    lieFilters,
    lieNextToken,
    lieMaxResults,
    lieInstanceId,
    lieTypeName,

    -- * Destructuring the response
    ListInventoryEntriesResponse (..),
    mkListInventoryEntriesResponse,

    -- ** Response lenses
    liersInstanceId,
    liersTypeName,
    liersEntries,
    liersSchemaVersion,
    liersCaptureTime,
    liersNextToken,
    liersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkListInventoryEntries' smart constructor.
data ListInventoryEntries = ListInventoryEntries'
  { filters ::
      Lude.Maybe (Lude.NonEmpty InventoryFilter),
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    instanceId :: Lude.Text,
    typeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInventoryEntries' with the minimum fields required to make a request.
--
-- * 'filters' - One or more filters. Use a filter to return a more specific list of results.
-- * 'instanceId' - The instance ID for which you want inventory information.
-- * 'maxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'typeName' - The type of inventory item for which you want information.
mkListInventoryEntries ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'typeName'
  Lude.Text ->
  ListInventoryEntries
mkListInventoryEntries pInstanceId_ pTypeName_ =
  ListInventoryEntries'
    { filters = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      instanceId = pInstanceId_,
      typeName = pTypeName_
    }

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieFilters :: Lens.Lens' ListInventoryEntries (Lude.Maybe (Lude.NonEmpty InventoryFilter))
lieFilters = Lens.lens (filters :: ListInventoryEntries -> Lude.Maybe (Lude.NonEmpty InventoryFilter)) (\s a -> s {filters = a} :: ListInventoryEntries)
{-# DEPRECATED lieFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieNextToken :: Lens.Lens' ListInventoryEntries (Lude.Maybe Lude.Text)
lieNextToken = Lens.lens (nextToken :: ListInventoryEntries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInventoryEntries)
{-# DEPRECATED lieNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieMaxResults :: Lens.Lens' ListInventoryEntries (Lude.Maybe Lude.Natural)
lieMaxResults = Lens.lens (maxResults :: ListInventoryEntries -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListInventoryEntries)
{-# DEPRECATED lieMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The instance ID for which you want inventory information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieInstanceId :: Lens.Lens' ListInventoryEntries Lude.Text
lieInstanceId = Lens.lens (instanceId :: ListInventoryEntries -> Lude.Text) (\s a -> s {instanceId = a} :: ListInventoryEntries)
{-# DEPRECATED lieInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of inventory item for which you want information.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lieTypeName :: Lens.Lens' ListInventoryEntries Lude.Text
lieTypeName = Lens.lens (typeName :: ListInventoryEntries -> Lude.Text) (\s a -> s {typeName = a} :: ListInventoryEntries)
{-# DEPRECATED lieTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

instance Lude.AWSRequest ListInventoryEntries where
  type Rs ListInventoryEntries = ListInventoryEntriesResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInventoryEntriesResponse'
            Lude.<$> (x Lude..?> "InstanceId")
            Lude.<*> (x Lude..?> "TypeName")
            Lude.<*> (x Lude..?> "Entries" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "SchemaVersion")
            Lude.<*> (x Lude..?> "CaptureTime")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInventoryEntries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.ListInventoryEntries" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInventoryEntries where
  toJSON ListInventoryEntries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("InstanceId" Lude..= instanceId),
            Lude.Just ("TypeName" Lude..= typeName)
          ]
      )

instance Lude.ToPath ListInventoryEntries where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInventoryEntries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListInventoryEntriesResponse' smart constructor.
data ListInventoryEntriesResponse = ListInventoryEntriesResponse'
  { instanceId ::
      Lude.Maybe Lude.Text,
    typeName :: Lude.Maybe Lude.Text,
    entries ::
      Lude.Maybe
        [ Lude.HashMap
            Lude.Text
            (Lude.Text)
        ],
    schemaVersion ::
      Lude.Maybe Lude.Text,
    captureTime ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListInventoryEntriesResponse' with the minimum fields required to make a request.
--
-- * 'captureTime' - The time that inventory information was collected for the instance(s).
-- * 'entries' - A list of inventory items on the instance(s).
-- * 'instanceId' - The instance ID targeted by the request to query inventory information.
-- * 'nextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
-- * 'responseStatus' - The response status code.
-- * 'schemaVersion' - The inventory schema version used by the instance(s).
-- * 'typeName' - The type of inventory item returned by the request.
mkListInventoryEntriesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInventoryEntriesResponse
mkListInventoryEntriesResponse pResponseStatus_ =
  ListInventoryEntriesResponse'
    { instanceId = Lude.Nothing,
      typeName = Lude.Nothing,
      entries = Lude.Nothing,
      schemaVersion = Lude.Nothing,
      captureTime = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The instance ID targeted by the request to query inventory information.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersInstanceId :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe Lude.Text)
liersInstanceId = Lens.lens (instanceId :: ListInventoryEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of inventory item returned by the request.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersTypeName :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe Lude.Text)
liersTypeName = Lens.lens (typeName :: ListInventoryEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | A list of inventory items on the instance(s).
--
-- /Note:/ Consider using 'entries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersEntries :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
liersEntries = Lens.lens (entries :: ListInventoryEntriesResponse -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {entries = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersEntries "Use generic-lens or generic-optics with 'entries' instead." #-}

-- | The inventory schema version used by the instance(s).
--
-- /Note:/ Consider using 'schemaVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersSchemaVersion :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe Lude.Text)
liersSchemaVersion = Lens.lens (schemaVersion :: ListInventoryEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {schemaVersion = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersSchemaVersion "Use generic-lens or generic-optics with 'schemaVersion' instead." #-}

-- | The time that inventory information was collected for the instance(s).
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersCaptureTime :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe Lude.Text)
liersCaptureTime = Lens.lens (captureTime :: ListInventoryEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {captureTime = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersNextToken :: Lens.Lens' ListInventoryEntriesResponse (Lude.Maybe Lude.Text)
liersNextToken = Lens.lens (nextToken :: ListInventoryEntriesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liersResponseStatus :: Lens.Lens' ListInventoryEntriesResponse Lude.Int
liersResponseStatus = Lens.lens (responseStatus :: ListInventoryEntriesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInventoryEntriesResponse)
{-# DEPRECATED liersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

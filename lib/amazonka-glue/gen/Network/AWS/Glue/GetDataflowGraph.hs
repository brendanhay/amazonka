{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetDataflowGraph
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a Python script into a directed acyclic graph (DAG).
--
--
module Network.AWS.Glue.GetDataflowGraph
    (
    -- * Creating a Request
      getDataflowGraph
    , GetDataflowGraph
    -- * Request Lenses
    , gdgPythonScript

    -- * Destructuring the Response
    , getDataflowGraphResponse
    , GetDataflowGraphResponse
    -- * Response Lenses
    , gdgrsDagEdges
    , gdgrsDagNodes
    , gdgrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDataflowGraph' smart constructor.
newtype GetDataflowGraph = GetDataflowGraph'
  { _gdgPythonScript :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataflowGraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdgPythonScript' - The Python script to transform.
getDataflowGraph
    :: GetDataflowGraph
getDataflowGraph = GetDataflowGraph' {_gdgPythonScript = Nothing}


-- | The Python script to transform.
gdgPythonScript :: Lens' GetDataflowGraph (Maybe Text)
gdgPythonScript = lens _gdgPythonScript (\ s a -> s{_gdgPythonScript = a})

instance AWSRequest GetDataflowGraph where
        type Rs GetDataflowGraph = GetDataflowGraphResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetDataflowGraphResponse' <$>
                   (x .?> "DagEdges" .!@ mempty) <*>
                     (x .?> "DagNodes" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetDataflowGraph where

instance NFData GetDataflowGraph where

instance ToHeaders GetDataflowGraph where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.GetDataflowGraph" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDataflowGraph where
        toJSON GetDataflowGraph'{..}
          = object
              (catMaybes
                 [("PythonScript" .=) <$> _gdgPythonScript])

instance ToPath GetDataflowGraph where
        toPath = const "/"

instance ToQuery GetDataflowGraph where
        toQuery = const mempty

-- | /See:/ 'getDataflowGraphResponse' smart constructor.
data GetDataflowGraphResponse = GetDataflowGraphResponse'
  { _gdgrsDagEdges       :: !(Maybe [CodeGenEdge])
  , _gdgrsDagNodes       :: !(Maybe [CodeGenNode])
  , _gdgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDataflowGraphResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdgrsDagEdges' - A list of the edges in the resulting DAG.
--
-- * 'gdgrsDagNodes' - A list of the nodes in the resulting DAG.
--
-- * 'gdgrsResponseStatus' - -- | The response status code.
getDataflowGraphResponse
    :: Int -- ^ 'gdgrsResponseStatus'
    -> GetDataflowGraphResponse
getDataflowGraphResponse pResponseStatus_ =
  GetDataflowGraphResponse'
    { _gdgrsDagEdges = Nothing
    , _gdgrsDagNodes = Nothing
    , _gdgrsResponseStatus = pResponseStatus_
    }


-- | A list of the edges in the resulting DAG.
gdgrsDagEdges :: Lens' GetDataflowGraphResponse [CodeGenEdge]
gdgrsDagEdges = lens _gdgrsDagEdges (\ s a -> s{_gdgrsDagEdges = a}) . _Default . _Coerce

-- | A list of the nodes in the resulting DAG.
gdgrsDagNodes :: Lens' GetDataflowGraphResponse [CodeGenNode]
gdgrsDagNodes = lens _gdgrsDagNodes (\ s a -> s{_gdgrsDagNodes = a}) . _Default . _Coerce

-- | -- | The response status code.
gdgrsResponseStatus :: Lens' GetDataflowGraphResponse Int
gdgrsResponseStatus = lens _gdgrsResponseStatus (\ s a -> s{_gdgrsResponseStatus = a})

instance NFData GetDataflowGraphResponse where

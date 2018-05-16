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
-- Module      : Network.AWS.Glue.CreateScript
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Transforms a directed acyclic graph (DAG) into code.
--
--
module Network.AWS.Glue.CreateScript
    (
    -- * Creating a Request
      createScript
    , CreateScript
    -- * Request Lenses
    , csDagEdges
    , csLanguage
    , csDagNodes

    -- * Destructuring the Response
    , createScriptResponse
    , CreateScriptResponse
    -- * Response Lenses
    , csrsPythonScript
    , csrsScalaCode
    , csrsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createScript' smart constructor.
data CreateScript = CreateScript'
  { _csDagEdges :: !(Maybe [CodeGenEdge])
  , _csLanguage :: !(Maybe Language)
  , _csDagNodes :: !(Maybe [CodeGenNode])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateScript' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csDagEdges' - A list of the edges in the DAG.
--
-- * 'csLanguage' - The programming language of the resulting code from the DAG.
--
-- * 'csDagNodes' - A list of the nodes in the DAG.
createScript
    :: CreateScript
createScript =
  CreateScript'
    {_csDagEdges = Nothing, _csLanguage = Nothing, _csDagNodes = Nothing}


-- | A list of the edges in the DAG.
csDagEdges :: Lens' CreateScript [CodeGenEdge]
csDagEdges = lens _csDagEdges (\ s a -> s{_csDagEdges = a}) . _Default . _Coerce

-- | The programming language of the resulting code from the DAG.
csLanguage :: Lens' CreateScript (Maybe Language)
csLanguage = lens _csLanguage (\ s a -> s{_csLanguage = a})

-- | A list of the nodes in the DAG.
csDagNodes :: Lens' CreateScript [CodeGenNode]
csDagNodes = lens _csDagNodes (\ s a -> s{_csDagNodes = a}) . _Default . _Coerce

instance AWSRequest CreateScript where
        type Rs CreateScript = CreateScriptResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 CreateScriptResponse' <$>
                   (x .?> "PythonScript") <*> (x .?> "ScalaCode") <*>
                     (pure (fromEnum s)))

instance Hashable CreateScript where

instance NFData CreateScript where

instance ToHeaders CreateScript where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateScript" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateScript where
        toJSON CreateScript'{..}
          = object
              (catMaybes
                 [("DagEdges" .=) <$> _csDagEdges,
                  ("Language" .=) <$> _csLanguage,
                  ("DagNodes" .=) <$> _csDagNodes])

instance ToPath CreateScript where
        toPath = const "/"

instance ToQuery CreateScript where
        toQuery = const mempty

-- | /See:/ 'createScriptResponse' smart constructor.
data CreateScriptResponse = CreateScriptResponse'
  { _csrsPythonScript   :: !(Maybe Text)
  , _csrsScalaCode      :: !(Maybe Text)
  , _csrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateScriptResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csrsPythonScript' - The Python script generated from the DAG.
--
-- * 'csrsScalaCode' - The Scala code generated from the DAG.
--
-- * 'csrsResponseStatus' - -- | The response status code.
createScriptResponse
    :: Int -- ^ 'csrsResponseStatus'
    -> CreateScriptResponse
createScriptResponse pResponseStatus_ =
  CreateScriptResponse'
    { _csrsPythonScript = Nothing
    , _csrsScalaCode = Nothing
    , _csrsResponseStatus = pResponseStatus_
    }


-- | The Python script generated from the DAG.
csrsPythonScript :: Lens' CreateScriptResponse (Maybe Text)
csrsPythonScript = lens _csrsPythonScript (\ s a -> s{_csrsPythonScript = a})

-- | The Scala code generated from the DAG.
csrsScalaCode :: Lens' CreateScriptResponse (Maybe Text)
csrsScalaCode = lens _csrsScalaCode (\ s a -> s{_csrsScalaCode = a})

-- | -- | The response status code.
csrsResponseStatus :: Lens' CreateScriptResponse Int
csrsResponseStatus = lens _csrsResponseStatus (\ s a -> s{_csrsResponseStatus = a})

instance NFData CreateScriptResponse where

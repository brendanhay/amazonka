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
-- Module      : Network.AWS.Glue.GetPlan
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets code to perform a specified mapping.
--
--
module Network.AWS.Glue.GetPlan
    (
    -- * Creating a Request
      getPlan
    , GetPlan
    -- * Request Lenses
    , gpSinks
    , gpLocation
    , gpLanguage
    , gpMapping
    , gpSource

    -- * Destructuring the Response
    , getPlanResponse
    , GetPlanResponse
    -- * Response Lenses
    , gpprsPythonScript
    , gpprsScalaCode
    , gpprsResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getPlan' smart constructor.
data GetPlan = GetPlan'
  { _gpSinks    :: !(Maybe [CatalogEntry])
  , _gpLocation :: !(Maybe Location)
  , _gpLanguage :: !(Maybe Language)
  , _gpMapping  :: ![MappingEntry]
  , _gpSource   :: !CatalogEntry
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPlan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpSinks' - The target tables.
--
-- * 'gpLocation' - Parameters for the mapping.
--
-- * 'gpLanguage' - The programming language of the code to perform the mapping.
--
-- * 'gpMapping' - The list of mappings from a source table to target tables.
--
-- * 'gpSource' - The source table.
getPlan
    :: CatalogEntry -- ^ 'gpSource'
    -> GetPlan
getPlan pSource_ =
  GetPlan'
    { _gpSinks = Nothing
    , _gpLocation = Nothing
    , _gpLanguage = Nothing
    , _gpMapping = mempty
    , _gpSource = pSource_
    }


-- | The target tables.
gpSinks :: Lens' GetPlan [CatalogEntry]
gpSinks = lens _gpSinks (\ s a -> s{_gpSinks = a}) . _Default . _Coerce

-- | Parameters for the mapping.
gpLocation :: Lens' GetPlan (Maybe Location)
gpLocation = lens _gpLocation (\ s a -> s{_gpLocation = a})

-- | The programming language of the code to perform the mapping.
gpLanguage :: Lens' GetPlan (Maybe Language)
gpLanguage = lens _gpLanguage (\ s a -> s{_gpLanguage = a})

-- | The list of mappings from a source table to target tables.
gpMapping :: Lens' GetPlan [MappingEntry]
gpMapping = lens _gpMapping (\ s a -> s{_gpMapping = a}) . _Coerce

-- | The source table.
gpSource :: Lens' GetPlan CatalogEntry
gpSource = lens _gpSource (\ s a -> s{_gpSource = a})

instance AWSRequest GetPlan where
        type Rs GetPlan = GetPlanResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 GetPlanResponse' <$>
                   (x .?> "PythonScript") <*> (x .?> "ScalaCode") <*>
                     (pure (fromEnum s)))

instance Hashable GetPlan where

instance NFData GetPlan where

instance ToHeaders GetPlan where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =# ("AWSGlue.GetPlan" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetPlan where
        toJSON GetPlan'{..}
          = object
              (catMaybes
                 [("Sinks" .=) <$> _gpSinks,
                  ("Location" .=) <$> _gpLocation,
                  ("Language" .=) <$> _gpLanguage,
                  Just ("Mapping" .= _gpMapping),
                  Just ("Source" .= _gpSource)])

instance ToPath GetPlan where
        toPath = const "/"

instance ToQuery GetPlan where
        toQuery = const mempty

-- | /See:/ 'getPlanResponse' smart constructor.
data GetPlanResponse = GetPlanResponse'
  { _gpprsPythonScript   :: !(Maybe Text)
  , _gpprsScalaCode      :: !(Maybe Text)
  , _gpprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetPlanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gpprsPythonScript' - A Python script to perform the mapping.
--
-- * 'gpprsScalaCode' - Scala code to perform the mapping.
--
-- * 'gpprsResponseStatus' - -- | The response status code.
getPlanResponse
    :: Int -- ^ 'gpprsResponseStatus'
    -> GetPlanResponse
getPlanResponse pResponseStatus_ =
  GetPlanResponse'
    { _gpprsPythonScript = Nothing
    , _gpprsScalaCode = Nothing
    , _gpprsResponseStatus = pResponseStatus_
    }


-- | A Python script to perform the mapping.
gpprsPythonScript :: Lens' GetPlanResponse (Maybe Text)
gpprsPythonScript = lens _gpprsPythonScript (\ s a -> s{_gpprsPythonScript = a})

-- | Scala code to perform the mapping.
gpprsScalaCode :: Lens' GetPlanResponse (Maybe Text)
gpprsScalaCode = lens _gpprsScalaCode (\ s a -> s{_gpprsScalaCode = a})

-- | -- | The response status code.
gpprsResponseStatus :: Lens' GetPlanResponse Int
gpprsResponseStatus = lens _gpprsResponseStatus (\ s a -> s{_gpprsResponseStatus = a})

instance NFData GetPlanResponse where

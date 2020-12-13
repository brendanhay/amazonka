{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchConstraintValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchConstraintValue
  ( GeoMatchConstraintValue
      ( GeoMatchConstraintValue',
        GMCVAF,
        GMCVAX,
        GMCVAL,
        GMCVDZ,
        GMCVAS,
        GMCVAD,
        GMCVAO,
        GMCVAI,
        GMCVAQ,
        GMCVAG,
        GMCVAR,
        GMCVAM,
        GMCVAW,
        GMCVAU,
        GMCVAT,
        GMCVAZ,
        GMCVBS,
        GMCVBH,
        GMCVBD,
        GMCVBB,
        GMCVBY,
        GMCVBE,
        GMCVBZ,
        GMCVBJ,
        GMCVBM,
        GMCVBT,
        GMCVBO,
        GMCVBQ,
        GMCVBA,
        GMCVBW,
        GMCVBV,
        GMCVBR,
        GMCVIO,
        GMCVBN,
        GMCVBG,
        GMCVBF,
        GMCVBI,
        GMCVKH,
        GMCVCM,
        GMCVCA,
        GMCVCV,
        GMCVKY,
        GMCVCF,
        GMCVTD,
        GMCVCL,
        GMCVCN,
        GMCVCX,
        GMCVCC,
        GMCVCO,
        GMCVKM,
        GMCVCG,
        GMCVCD,
        GMCVCK,
        GMCVCR,
        GMCVCI,
        GMCVHR,
        GMCVCU,
        GMCVCW,
        GMCVCY,
        GMCVCZ,
        GMCVDK,
        GMCVDJ,
        GMCVDM,
        GMCVDO,
        GMCVEC,
        GMCVEG,
        GMCVSV,
        GMCVGQ,
        GMCVER,
        GMCVEE,
        GMCVET,
        GMCVFK,
        GMCVFO,
        GMCVFJ,
        GMCVFI,
        GMCVFR,
        GMCVGF,
        GMCVPF,
        GMCVTF,
        GMCVGA,
        GMCVGM,
        GMCVGE,
        GMCVDE,
        GMCVGH,
        GMCVGI,
        GMCVGR,
        GMCVGL,
        GMCVGD,
        GMCVGP,
        GMCVGU,
        GMCVGT,
        GMCVGG,
        GMCVGN,
        GMCVGW,
        GMCVGY,
        GMCVHT,
        GMCVHM,
        GMCVVA,
        GMCVHN,
        GMCVHK,
        GMCVHU,
        GMCVIS,
        GMCVIN,
        GMCVId,
        GMCVIR,
        GMCVIQ,
        GMCVIE,
        GMCVIM,
        GMCVIL,
        GMCVIT,
        GMCVJM,
        GMCVJP,
        GMCVJE,
        GMCVJO,
        GMCVKZ,
        GMCVKE,
        GMCVKI,
        GMCVKP,
        GMCVKR,
        GMCVKW,
        GMCVKG,
        GMCVLA,
        GMCVLV,
        GMCVLB,
        GMCVLS,
        GMCVLR,
        GMCVLY,
        GMCVLI,
        GMCVLT,
        GMCVLU,
        GMCVMO,
        GMCVMK,
        GMCVMG,
        GMCVMW,
        GMCVMY,
        GMCVMV,
        GMCVML,
        GMCVMT,
        GMCVMH,
        GMCVMQ,
        GMCVMR,
        GMCVMU,
        GMCVYT,
        GMCVMX,
        GMCVFM,
        GMCVMD,
        GMCVMC,
        GMCVMN,
        GMCVME,
        GMCVMS,
        GMCVMA,
        GMCVMZ,
        GMCVMM,
        GMCVNA,
        GMCVNR,
        GMCVNP,
        GMCVNL,
        GMCVNC,
        GMCVNZ,
        GMCVNI,
        GMCVNE,
        GMCVNG,
        GMCVNU,
        GMCVNF,
        GMCVMP,
        GMCVNO,
        GMCVOM,
        GMCVPK,
        GMCVPW,
        GMCVPS,
        GMCVPA,
        GMCVPG,
        GMCVPY,
        GMCVPE,
        GMCVPH,
        GMCVPN,
        GMCVPL,
        GMCVPT,
        GMCVPR,
        GMCVQA,
        GMCVRE,
        GMCVRO,
        GMCVRU,
        GMCVRW,
        GMCVBL,
        GMCVSH,
        GMCVKN,
        GMCVLC,
        GMCVMF,
        GMCVPM,
        GMCVVC,
        GMCVWS,
        GMCVSM,
        GMCVST,
        GMCVSA,
        GMCVSN,
        GMCVRS,
        GMCVSC,
        GMCVSL,
        GMCVSG,
        GMCVSX,
        GMCVSK,
        GMCVSI,
        GMCVSB,
        GMCVSO,
        GMCVZA,
        GMCVGS,
        GMCVSS,
        GMCVES,
        GMCVLK,
        GMCVSD,
        GMCVSR,
        GMCVSJ,
        GMCVSZ,
        GMCVSE,
        GMCVCH,
        GMCVSY,
        GMCVTW,
        GMCVTJ,
        GMCVTZ,
        GMCVTH,
        GMCVTL,
        GMCVTG,
        GMCVTK,
        GMCVTO,
        GMCVTT,
        GMCVTN,
        GMCVTR,
        GMCVTM,
        GMCVTC,
        GMCVTV,
        GMCVUG,
        GMCVUA,
        GMCVAE,
        GMCVGB,
        GMCVUS,
        GMCVUM,
        GMCVUY,
        GMCVUZ,
        GMCVVU,
        GMCVVE,
        GMCVVN,
        GMCVVG,
        GMCVVI,
        GMCVWF,
        GMCVEH,
        GMCVYE,
        GMCVZM,
        GMCVZW
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GeoMatchConstraintValue = GeoMatchConstraintValue' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern GMCVAF :: GeoMatchConstraintValue
pattern GMCVAF = GeoMatchConstraintValue' "AF"

pattern GMCVAX :: GeoMatchConstraintValue
pattern GMCVAX = GeoMatchConstraintValue' "AX"

pattern GMCVAL :: GeoMatchConstraintValue
pattern GMCVAL = GeoMatchConstraintValue' "AL"

pattern GMCVDZ :: GeoMatchConstraintValue
pattern GMCVDZ = GeoMatchConstraintValue' "DZ"

pattern GMCVAS :: GeoMatchConstraintValue
pattern GMCVAS = GeoMatchConstraintValue' "AS"

pattern GMCVAD :: GeoMatchConstraintValue
pattern GMCVAD = GeoMatchConstraintValue' "AD"

pattern GMCVAO :: GeoMatchConstraintValue
pattern GMCVAO = GeoMatchConstraintValue' "AO"

pattern GMCVAI :: GeoMatchConstraintValue
pattern GMCVAI = GeoMatchConstraintValue' "AI"

pattern GMCVAQ :: GeoMatchConstraintValue
pattern GMCVAQ = GeoMatchConstraintValue' "AQ"

pattern GMCVAG :: GeoMatchConstraintValue
pattern GMCVAG = GeoMatchConstraintValue' "AG"

pattern GMCVAR :: GeoMatchConstraintValue
pattern GMCVAR = GeoMatchConstraintValue' "AR"

pattern GMCVAM :: GeoMatchConstraintValue
pattern GMCVAM = GeoMatchConstraintValue' "AM"

pattern GMCVAW :: GeoMatchConstraintValue
pattern GMCVAW = GeoMatchConstraintValue' "AW"

pattern GMCVAU :: GeoMatchConstraintValue
pattern GMCVAU = GeoMatchConstraintValue' "AU"

pattern GMCVAT :: GeoMatchConstraintValue
pattern GMCVAT = GeoMatchConstraintValue' "AT"

pattern GMCVAZ :: GeoMatchConstraintValue
pattern GMCVAZ = GeoMatchConstraintValue' "AZ"

pattern GMCVBS :: GeoMatchConstraintValue
pattern GMCVBS = GeoMatchConstraintValue' "BS"

pattern GMCVBH :: GeoMatchConstraintValue
pattern GMCVBH = GeoMatchConstraintValue' "BH"

pattern GMCVBD :: GeoMatchConstraintValue
pattern GMCVBD = GeoMatchConstraintValue' "BD"

pattern GMCVBB :: GeoMatchConstraintValue
pattern GMCVBB = GeoMatchConstraintValue' "BB"

pattern GMCVBY :: GeoMatchConstraintValue
pattern GMCVBY = GeoMatchConstraintValue' "BY"

pattern GMCVBE :: GeoMatchConstraintValue
pattern GMCVBE = GeoMatchConstraintValue' "BE"

pattern GMCVBZ :: GeoMatchConstraintValue
pattern GMCVBZ = GeoMatchConstraintValue' "BZ"

pattern GMCVBJ :: GeoMatchConstraintValue
pattern GMCVBJ = GeoMatchConstraintValue' "BJ"

pattern GMCVBM :: GeoMatchConstraintValue
pattern GMCVBM = GeoMatchConstraintValue' "BM"

pattern GMCVBT :: GeoMatchConstraintValue
pattern GMCVBT = GeoMatchConstraintValue' "BT"

pattern GMCVBO :: GeoMatchConstraintValue
pattern GMCVBO = GeoMatchConstraintValue' "BO"

pattern GMCVBQ :: GeoMatchConstraintValue
pattern GMCVBQ = GeoMatchConstraintValue' "BQ"

pattern GMCVBA :: GeoMatchConstraintValue
pattern GMCVBA = GeoMatchConstraintValue' "BA"

pattern GMCVBW :: GeoMatchConstraintValue
pattern GMCVBW = GeoMatchConstraintValue' "BW"

pattern GMCVBV :: GeoMatchConstraintValue
pattern GMCVBV = GeoMatchConstraintValue' "BV"

pattern GMCVBR :: GeoMatchConstraintValue
pattern GMCVBR = GeoMatchConstraintValue' "BR"

pattern GMCVIO :: GeoMatchConstraintValue
pattern GMCVIO = GeoMatchConstraintValue' "IO"

pattern GMCVBN :: GeoMatchConstraintValue
pattern GMCVBN = GeoMatchConstraintValue' "BN"

pattern GMCVBG :: GeoMatchConstraintValue
pattern GMCVBG = GeoMatchConstraintValue' "BG"

pattern GMCVBF :: GeoMatchConstraintValue
pattern GMCVBF = GeoMatchConstraintValue' "BF"

pattern GMCVBI :: GeoMatchConstraintValue
pattern GMCVBI = GeoMatchConstraintValue' "BI"

pattern GMCVKH :: GeoMatchConstraintValue
pattern GMCVKH = GeoMatchConstraintValue' "KH"

pattern GMCVCM :: GeoMatchConstraintValue
pattern GMCVCM = GeoMatchConstraintValue' "CM"

pattern GMCVCA :: GeoMatchConstraintValue
pattern GMCVCA = GeoMatchConstraintValue' "CA"

pattern GMCVCV :: GeoMatchConstraintValue
pattern GMCVCV = GeoMatchConstraintValue' "CV"

pattern GMCVKY :: GeoMatchConstraintValue
pattern GMCVKY = GeoMatchConstraintValue' "KY"

pattern GMCVCF :: GeoMatchConstraintValue
pattern GMCVCF = GeoMatchConstraintValue' "CF"

pattern GMCVTD :: GeoMatchConstraintValue
pattern GMCVTD = GeoMatchConstraintValue' "TD"

pattern GMCVCL :: GeoMatchConstraintValue
pattern GMCVCL = GeoMatchConstraintValue' "CL"

pattern GMCVCN :: GeoMatchConstraintValue
pattern GMCVCN = GeoMatchConstraintValue' "CN"

pattern GMCVCX :: GeoMatchConstraintValue
pattern GMCVCX = GeoMatchConstraintValue' "CX"

pattern GMCVCC :: GeoMatchConstraintValue
pattern GMCVCC = GeoMatchConstraintValue' "CC"

pattern GMCVCO :: GeoMatchConstraintValue
pattern GMCVCO = GeoMatchConstraintValue' "CO"

pattern GMCVKM :: GeoMatchConstraintValue
pattern GMCVKM = GeoMatchConstraintValue' "KM"

pattern GMCVCG :: GeoMatchConstraintValue
pattern GMCVCG = GeoMatchConstraintValue' "CG"

pattern GMCVCD :: GeoMatchConstraintValue
pattern GMCVCD = GeoMatchConstraintValue' "CD"

pattern GMCVCK :: GeoMatchConstraintValue
pattern GMCVCK = GeoMatchConstraintValue' "CK"

pattern GMCVCR :: GeoMatchConstraintValue
pattern GMCVCR = GeoMatchConstraintValue' "CR"

pattern GMCVCI :: GeoMatchConstraintValue
pattern GMCVCI = GeoMatchConstraintValue' "CI"

pattern GMCVHR :: GeoMatchConstraintValue
pattern GMCVHR = GeoMatchConstraintValue' "HR"

pattern GMCVCU :: GeoMatchConstraintValue
pattern GMCVCU = GeoMatchConstraintValue' "CU"

pattern GMCVCW :: GeoMatchConstraintValue
pattern GMCVCW = GeoMatchConstraintValue' "CW"

pattern GMCVCY :: GeoMatchConstraintValue
pattern GMCVCY = GeoMatchConstraintValue' "CY"

pattern GMCVCZ :: GeoMatchConstraintValue
pattern GMCVCZ = GeoMatchConstraintValue' "CZ"

pattern GMCVDK :: GeoMatchConstraintValue
pattern GMCVDK = GeoMatchConstraintValue' "DK"

pattern GMCVDJ :: GeoMatchConstraintValue
pattern GMCVDJ = GeoMatchConstraintValue' "DJ"

pattern GMCVDM :: GeoMatchConstraintValue
pattern GMCVDM = GeoMatchConstraintValue' "DM"

pattern GMCVDO :: GeoMatchConstraintValue
pattern GMCVDO = GeoMatchConstraintValue' "DO"

pattern GMCVEC :: GeoMatchConstraintValue
pattern GMCVEC = GeoMatchConstraintValue' "EC"

pattern GMCVEG :: GeoMatchConstraintValue
pattern GMCVEG = GeoMatchConstraintValue' "EG"

pattern GMCVSV :: GeoMatchConstraintValue
pattern GMCVSV = GeoMatchConstraintValue' "SV"

pattern GMCVGQ :: GeoMatchConstraintValue
pattern GMCVGQ = GeoMatchConstraintValue' "GQ"

pattern GMCVER :: GeoMatchConstraintValue
pattern GMCVER = GeoMatchConstraintValue' "ER"

pattern GMCVEE :: GeoMatchConstraintValue
pattern GMCVEE = GeoMatchConstraintValue' "EE"

pattern GMCVET :: GeoMatchConstraintValue
pattern GMCVET = GeoMatchConstraintValue' "ET"

pattern GMCVFK :: GeoMatchConstraintValue
pattern GMCVFK = GeoMatchConstraintValue' "FK"

pattern GMCVFO :: GeoMatchConstraintValue
pattern GMCVFO = GeoMatchConstraintValue' "FO"

pattern GMCVFJ :: GeoMatchConstraintValue
pattern GMCVFJ = GeoMatchConstraintValue' "FJ"

pattern GMCVFI :: GeoMatchConstraintValue
pattern GMCVFI = GeoMatchConstraintValue' "FI"

pattern GMCVFR :: GeoMatchConstraintValue
pattern GMCVFR = GeoMatchConstraintValue' "FR"

pattern GMCVGF :: GeoMatchConstraintValue
pattern GMCVGF = GeoMatchConstraintValue' "GF"

pattern GMCVPF :: GeoMatchConstraintValue
pattern GMCVPF = GeoMatchConstraintValue' "PF"

pattern GMCVTF :: GeoMatchConstraintValue
pattern GMCVTF = GeoMatchConstraintValue' "TF"

pattern GMCVGA :: GeoMatchConstraintValue
pattern GMCVGA = GeoMatchConstraintValue' "GA"

pattern GMCVGM :: GeoMatchConstraintValue
pattern GMCVGM = GeoMatchConstraintValue' "GM"

pattern GMCVGE :: GeoMatchConstraintValue
pattern GMCVGE = GeoMatchConstraintValue' "GE"

pattern GMCVDE :: GeoMatchConstraintValue
pattern GMCVDE = GeoMatchConstraintValue' "DE"

pattern GMCVGH :: GeoMatchConstraintValue
pattern GMCVGH = GeoMatchConstraintValue' "GH"

pattern GMCVGI :: GeoMatchConstraintValue
pattern GMCVGI = GeoMatchConstraintValue' "GI"

pattern GMCVGR :: GeoMatchConstraintValue
pattern GMCVGR = GeoMatchConstraintValue' "GR"

pattern GMCVGL :: GeoMatchConstraintValue
pattern GMCVGL = GeoMatchConstraintValue' "GL"

pattern GMCVGD :: GeoMatchConstraintValue
pattern GMCVGD = GeoMatchConstraintValue' "GD"

pattern GMCVGP :: GeoMatchConstraintValue
pattern GMCVGP = GeoMatchConstraintValue' "GP"

pattern GMCVGU :: GeoMatchConstraintValue
pattern GMCVGU = GeoMatchConstraintValue' "GU"

pattern GMCVGT :: GeoMatchConstraintValue
pattern GMCVGT = GeoMatchConstraintValue' "GT"

pattern GMCVGG :: GeoMatchConstraintValue
pattern GMCVGG = GeoMatchConstraintValue' "GG"

pattern GMCVGN :: GeoMatchConstraintValue
pattern GMCVGN = GeoMatchConstraintValue' "GN"

pattern GMCVGW :: GeoMatchConstraintValue
pattern GMCVGW = GeoMatchConstraintValue' "GW"

pattern GMCVGY :: GeoMatchConstraintValue
pattern GMCVGY = GeoMatchConstraintValue' "GY"

pattern GMCVHT :: GeoMatchConstraintValue
pattern GMCVHT = GeoMatchConstraintValue' "HT"

pattern GMCVHM :: GeoMatchConstraintValue
pattern GMCVHM = GeoMatchConstraintValue' "HM"

pattern GMCVVA :: GeoMatchConstraintValue
pattern GMCVVA = GeoMatchConstraintValue' "VA"

pattern GMCVHN :: GeoMatchConstraintValue
pattern GMCVHN = GeoMatchConstraintValue' "HN"

pattern GMCVHK :: GeoMatchConstraintValue
pattern GMCVHK = GeoMatchConstraintValue' "HK"

pattern GMCVHU :: GeoMatchConstraintValue
pattern GMCVHU = GeoMatchConstraintValue' "HU"

pattern GMCVIS :: GeoMatchConstraintValue
pattern GMCVIS = GeoMatchConstraintValue' "IS"

pattern GMCVIN :: GeoMatchConstraintValue
pattern GMCVIN = GeoMatchConstraintValue' "IN"

pattern GMCVId :: GeoMatchConstraintValue
pattern GMCVId = GeoMatchConstraintValue' "ID"

pattern GMCVIR :: GeoMatchConstraintValue
pattern GMCVIR = GeoMatchConstraintValue' "IR"

pattern GMCVIQ :: GeoMatchConstraintValue
pattern GMCVIQ = GeoMatchConstraintValue' "IQ"

pattern GMCVIE :: GeoMatchConstraintValue
pattern GMCVIE = GeoMatchConstraintValue' "IE"

pattern GMCVIM :: GeoMatchConstraintValue
pattern GMCVIM = GeoMatchConstraintValue' "IM"

pattern GMCVIL :: GeoMatchConstraintValue
pattern GMCVIL = GeoMatchConstraintValue' "IL"

pattern GMCVIT :: GeoMatchConstraintValue
pattern GMCVIT = GeoMatchConstraintValue' "IT"

pattern GMCVJM :: GeoMatchConstraintValue
pattern GMCVJM = GeoMatchConstraintValue' "JM"

pattern GMCVJP :: GeoMatchConstraintValue
pattern GMCVJP = GeoMatchConstraintValue' "JP"

pattern GMCVJE :: GeoMatchConstraintValue
pattern GMCVJE = GeoMatchConstraintValue' "JE"

pattern GMCVJO :: GeoMatchConstraintValue
pattern GMCVJO = GeoMatchConstraintValue' "JO"

pattern GMCVKZ :: GeoMatchConstraintValue
pattern GMCVKZ = GeoMatchConstraintValue' "KZ"

pattern GMCVKE :: GeoMatchConstraintValue
pattern GMCVKE = GeoMatchConstraintValue' "KE"

pattern GMCVKI :: GeoMatchConstraintValue
pattern GMCVKI = GeoMatchConstraintValue' "KI"

pattern GMCVKP :: GeoMatchConstraintValue
pattern GMCVKP = GeoMatchConstraintValue' "KP"

pattern GMCVKR :: GeoMatchConstraintValue
pattern GMCVKR = GeoMatchConstraintValue' "KR"

pattern GMCVKW :: GeoMatchConstraintValue
pattern GMCVKW = GeoMatchConstraintValue' "KW"

pattern GMCVKG :: GeoMatchConstraintValue
pattern GMCVKG = GeoMatchConstraintValue' "KG"

pattern GMCVLA :: GeoMatchConstraintValue
pattern GMCVLA = GeoMatchConstraintValue' "LA"

pattern GMCVLV :: GeoMatchConstraintValue
pattern GMCVLV = GeoMatchConstraintValue' "LV"

pattern GMCVLB :: GeoMatchConstraintValue
pattern GMCVLB = GeoMatchConstraintValue' "LB"

pattern GMCVLS :: GeoMatchConstraintValue
pattern GMCVLS = GeoMatchConstraintValue' "LS"

pattern GMCVLR :: GeoMatchConstraintValue
pattern GMCVLR = GeoMatchConstraintValue' "LR"

pattern GMCVLY :: GeoMatchConstraintValue
pattern GMCVLY = GeoMatchConstraintValue' "LY"

pattern GMCVLI :: GeoMatchConstraintValue
pattern GMCVLI = GeoMatchConstraintValue' "LI"

pattern GMCVLT :: GeoMatchConstraintValue
pattern GMCVLT = GeoMatchConstraintValue' "LT"

pattern GMCVLU :: GeoMatchConstraintValue
pattern GMCVLU = GeoMatchConstraintValue' "LU"

pattern GMCVMO :: GeoMatchConstraintValue
pattern GMCVMO = GeoMatchConstraintValue' "MO"

pattern GMCVMK :: GeoMatchConstraintValue
pattern GMCVMK = GeoMatchConstraintValue' "MK"

pattern GMCVMG :: GeoMatchConstraintValue
pattern GMCVMG = GeoMatchConstraintValue' "MG"

pattern GMCVMW :: GeoMatchConstraintValue
pattern GMCVMW = GeoMatchConstraintValue' "MW"

pattern GMCVMY :: GeoMatchConstraintValue
pattern GMCVMY = GeoMatchConstraintValue' "MY"

pattern GMCVMV :: GeoMatchConstraintValue
pattern GMCVMV = GeoMatchConstraintValue' "MV"

pattern GMCVML :: GeoMatchConstraintValue
pattern GMCVML = GeoMatchConstraintValue' "ML"

pattern GMCVMT :: GeoMatchConstraintValue
pattern GMCVMT = GeoMatchConstraintValue' "MT"

pattern GMCVMH :: GeoMatchConstraintValue
pattern GMCVMH = GeoMatchConstraintValue' "MH"

pattern GMCVMQ :: GeoMatchConstraintValue
pattern GMCVMQ = GeoMatchConstraintValue' "MQ"

pattern GMCVMR :: GeoMatchConstraintValue
pattern GMCVMR = GeoMatchConstraintValue' "MR"

pattern GMCVMU :: GeoMatchConstraintValue
pattern GMCVMU = GeoMatchConstraintValue' "MU"

pattern GMCVYT :: GeoMatchConstraintValue
pattern GMCVYT = GeoMatchConstraintValue' "YT"

pattern GMCVMX :: GeoMatchConstraintValue
pattern GMCVMX = GeoMatchConstraintValue' "MX"

pattern GMCVFM :: GeoMatchConstraintValue
pattern GMCVFM = GeoMatchConstraintValue' "FM"

pattern GMCVMD :: GeoMatchConstraintValue
pattern GMCVMD = GeoMatchConstraintValue' "MD"

pattern GMCVMC :: GeoMatchConstraintValue
pattern GMCVMC = GeoMatchConstraintValue' "MC"

pattern GMCVMN :: GeoMatchConstraintValue
pattern GMCVMN = GeoMatchConstraintValue' "MN"

pattern GMCVME :: GeoMatchConstraintValue
pattern GMCVME = GeoMatchConstraintValue' "ME"

pattern GMCVMS :: GeoMatchConstraintValue
pattern GMCVMS = GeoMatchConstraintValue' "MS"

pattern GMCVMA :: GeoMatchConstraintValue
pattern GMCVMA = GeoMatchConstraintValue' "MA"

pattern GMCVMZ :: GeoMatchConstraintValue
pattern GMCVMZ = GeoMatchConstraintValue' "MZ"

pattern GMCVMM :: GeoMatchConstraintValue
pattern GMCVMM = GeoMatchConstraintValue' "MM"

pattern GMCVNA :: GeoMatchConstraintValue
pattern GMCVNA = GeoMatchConstraintValue' "NA"

pattern GMCVNR :: GeoMatchConstraintValue
pattern GMCVNR = GeoMatchConstraintValue' "NR"

pattern GMCVNP :: GeoMatchConstraintValue
pattern GMCVNP = GeoMatchConstraintValue' "NP"

pattern GMCVNL :: GeoMatchConstraintValue
pattern GMCVNL = GeoMatchConstraintValue' "NL"

pattern GMCVNC :: GeoMatchConstraintValue
pattern GMCVNC = GeoMatchConstraintValue' "NC"

pattern GMCVNZ :: GeoMatchConstraintValue
pattern GMCVNZ = GeoMatchConstraintValue' "NZ"

pattern GMCVNI :: GeoMatchConstraintValue
pattern GMCVNI = GeoMatchConstraintValue' "NI"

pattern GMCVNE :: GeoMatchConstraintValue
pattern GMCVNE = GeoMatchConstraintValue' "NE"

pattern GMCVNG :: GeoMatchConstraintValue
pattern GMCVNG = GeoMatchConstraintValue' "NG"

pattern GMCVNU :: GeoMatchConstraintValue
pattern GMCVNU = GeoMatchConstraintValue' "NU"

pattern GMCVNF :: GeoMatchConstraintValue
pattern GMCVNF = GeoMatchConstraintValue' "NF"

pattern GMCVMP :: GeoMatchConstraintValue
pattern GMCVMP = GeoMatchConstraintValue' "MP"

pattern GMCVNO :: GeoMatchConstraintValue
pattern GMCVNO = GeoMatchConstraintValue' "NO"

pattern GMCVOM :: GeoMatchConstraintValue
pattern GMCVOM = GeoMatchConstraintValue' "OM"

pattern GMCVPK :: GeoMatchConstraintValue
pattern GMCVPK = GeoMatchConstraintValue' "PK"

pattern GMCVPW :: GeoMatchConstraintValue
pattern GMCVPW = GeoMatchConstraintValue' "PW"

pattern GMCVPS :: GeoMatchConstraintValue
pattern GMCVPS = GeoMatchConstraintValue' "PS"

pattern GMCVPA :: GeoMatchConstraintValue
pattern GMCVPA = GeoMatchConstraintValue' "PA"

pattern GMCVPG :: GeoMatchConstraintValue
pattern GMCVPG = GeoMatchConstraintValue' "PG"

pattern GMCVPY :: GeoMatchConstraintValue
pattern GMCVPY = GeoMatchConstraintValue' "PY"

pattern GMCVPE :: GeoMatchConstraintValue
pattern GMCVPE = GeoMatchConstraintValue' "PE"

pattern GMCVPH :: GeoMatchConstraintValue
pattern GMCVPH = GeoMatchConstraintValue' "PH"

pattern GMCVPN :: GeoMatchConstraintValue
pattern GMCVPN = GeoMatchConstraintValue' "PN"

pattern GMCVPL :: GeoMatchConstraintValue
pattern GMCVPL = GeoMatchConstraintValue' "PL"

pattern GMCVPT :: GeoMatchConstraintValue
pattern GMCVPT = GeoMatchConstraintValue' "PT"

pattern GMCVPR :: GeoMatchConstraintValue
pattern GMCVPR = GeoMatchConstraintValue' "PR"

pattern GMCVQA :: GeoMatchConstraintValue
pattern GMCVQA = GeoMatchConstraintValue' "QA"

pattern GMCVRE :: GeoMatchConstraintValue
pattern GMCVRE = GeoMatchConstraintValue' "RE"

pattern GMCVRO :: GeoMatchConstraintValue
pattern GMCVRO = GeoMatchConstraintValue' "RO"

pattern GMCVRU :: GeoMatchConstraintValue
pattern GMCVRU = GeoMatchConstraintValue' "RU"

pattern GMCVRW :: GeoMatchConstraintValue
pattern GMCVRW = GeoMatchConstraintValue' "RW"

pattern GMCVBL :: GeoMatchConstraintValue
pattern GMCVBL = GeoMatchConstraintValue' "BL"

pattern GMCVSH :: GeoMatchConstraintValue
pattern GMCVSH = GeoMatchConstraintValue' "SH"

pattern GMCVKN :: GeoMatchConstraintValue
pattern GMCVKN = GeoMatchConstraintValue' "KN"

pattern GMCVLC :: GeoMatchConstraintValue
pattern GMCVLC = GeoMatchConstraintValue' "LC"

pattern GMCVMF :: GeoMatchConstraintValue
pattern GMCVMF = GeoMatchConstraintValue' "MF"

pattern GMCVPM :: GeoMatchConstraintValue
pattern GMCVPM = GeoMatchConstraintValue' "PM"

pattern GMCVVC :: GeoMatchConstraintValue
pattern GMCVVC = GeoMatchConstraintValue' "VC"

pattern GMCVWS :: GeoMatchConstraintValue
pattern GMCVWS = GeoMatchConstraintValue' "WS"

pattern GMCVSM :: GeoMatchConstraintValue
pattern GMCVSM = GeoMatchConstraintValue' "SM"

pattern GMCVST :: GeoMatchConstraintValue
pattern GMCVST = GeoMatchConstraintValue' "ST"

pattern GMCVSA :: GeoMatchConstraintValue
pattern GMCVSA = GeoMatchConstraintValue' "SA"

pattern GMCVSN :: GeoMatchConstraintValue
pattern GMCVSN = GeoMatchConstraintValue' "SN"

pattern GMCVRS :: GeoMatchConstraintValue
pattern GMCVRS = GeoMatchConstraintValue' "RS"

pattern GMCVSC :: GeoMatchConstraintValue
pattern GMCVSC = GeoMatchConstraintValue' "SC"

pattern GMCVSL :: GeoMatchConstraintValue
pattern GMCVSL = GeoMatchConstraintValue' "SL"

pattern GMCVSG :: GeoMatchConstraintValue
pattern GMCVSG = GeoMatchConstraintValue' "SG"

pattern GMCVSX :: GeoMatchConstraintValue
pattern GMCVSX = GeoMatchConstraintValue' "SX"

pattern GMCVSK :: GeoMatchConstraintValue
pattern GMCVSK = GeoMatchConstraintValue' "SK"

pattern GMCVSI :: GeoMatchConstraintValue
pattern GMCVSI = GeoMatchConstraintValue' "SI"

pattern GMCVSB :: GeoMatchConstraintValue
pattern GMCVSB = GeoMatchConstraintValue' "SB"

pattern GMCVSO :: GeoMatchConstraintValue
pattern GMCVSO = GeoMatchConstraintValue' "SO"

pattern GMCVZA :: GeoMatchConstraintValue
pattern GMCVZA = GeoMatchConstraintValue' "ZA"

pattern GMCVGS :: GeoMatchConstraintValue
pattern GMCVGS = GeoMatchConstraintValue' "GS"

pattern GMCVSS :: GeoMatchConstraintValue
pattern GMCVSS = GeoMatchConstraintValue' "SS"

pattern GMCVES :: GeoMatchConstraintValue
pattern GMCVES = GeoMatchConstraintValue' "ES"

pattern GMCVLK :: GeoMatchConstraintValue
pattern GMCVLK = GeoMatchConstraintValue' "LK"

pattern GMCVSD :: GeoMatchConstraintValue
pattern GMCVSD = GeoMatchConstraintValue' "SD"

pattern GMCVSR :: GeoMatchConstraintValue
pattern GMCVSR = GeoMatchConstraintValue' "SR"

pattern GMCVSJ :: GeoMatchConstraintValue
pattern GMCVSJ = GeoMatchConstraintValue' "SJ"

pattern GMCVSZ :: GeoMatchConstraintValue
pattern GMCVSZ = GeoMatchConstraintValue' "SZ"

pattern GMCVSE :: GeoMatchConstraintValue
pattern GMCVSE = GeoMatchConstraintValue' "SE"

pattern GMCVCH :: GeoMatchConstraintValue
pattern GMCVCH = GeoMatchConstraintValue' "CH"

pattern GMCVSY :: GeoMatchConstraintValue
pattern GMCVSY = GeoMatchConstraintValue' "SY"

pattern GMCVTW :: GeoMatchConstraintValue
pattern GMCVTW = GeoMatchConstraintValue' "TW"

pattern GMCVTJ :: GeoMatchConstraintValue
pattern GMCVTJ = GeoMatchConstraintValue' "TJ"

pattern GMCVTZ :: GeoMatchConstraintValue
pattern GMCVTZ = GeoMatchConstraintValue' "TZ"

pattern GMCVTH :: GeoMatchConstraintValue
pattern GMCVTH = GeoMatchConstraintValue' "TH"

pattern GMCVTL :: GeoMatchConstraintValue
pattern GMCVTL = GeoMatchConstraintValue' "TL"

pattern GMCVTG :: GeoMatchConstraintValue
pattern GMCVTG = GeoMatchConstraintValue' "TG"

pattern GMCVTK :: GeoMatchConstraintValue
pattern GMCVTK = GeoMatchConstraintValue' "TK"

pattern GMCVTO :: GeoMatchConstraintValue
pattern GMCVTO = GeoMatchConstraintValue' "TO"

pattern GMCVTT :: GeoMatchConstraintValue
pattern GMCVTT = GeoMatchConstraintValue' "TT"

pattern GMCVTN :: GeoMatchConstraintValue
pattern GMCVTN = GeoMatchConstraintValue' "TN"

pattern GMCVTR :: GeoMatchConstraintValue
pattern GMCVTR = GeoMatchConstraintValue' "TR"

pattern GMCVTM :: GeoMatchConstraintValue
pattern GMCVTM = GeoMatchConstraintValue' "TM"

pattern GMCVTC :: GeoMatchConstraintValue
pattern GMCVTC = GeoMatchConstraintValue' "TC"

pattern GMCVTV :: GeoMatchConstraintValue
pattern GMCVTV = GeoMatchConstraintValue' "TV"

pattern GMCVUG :: GeoMatchConstraintValue
pattern GMCVUG = GeoMatchConstraintValue' "UG"

pattern GMCVUA :: GeoMatchConstraintValue
pattern GMCVUA = GeoMatchConstraintValue' "UA"

pattern GMCVAE :: GeoMatchConstraintValue
pattern GMCVAE = GeoMatchConstraintValue' "AE"

pattern GMCVGB :: GeoMatchConstraintValue
pattern GMCVGB = GeoMatchConstraintValue' "GB"

pattern GMCVUS :: GeoMatchConstraintValue
pattern GMCVUS = GeoMatchConstraintValue' "US"

pattern GMCVUM :: GeoMatchConstraintValue
pattern GMCVUM = GeoMatchConstraintValue' "UM"

pattern GMCVUY :: GeoMatchConstraintValue
pattern GMCVUY = GeoMatchConstraintValue' "UY"

pattern GMCVUZ :: GeoMatchConstraintValue
pattern GMCVUZ = GeoMatchConstraintValue' "UZ"

pattern GMCVVU :: GeoMatchConstraintValue
pattern GMCVVU = GeoMatchConstraintValue' "VU"

pattern GMCVVE :: GeoMatchConstraintValue
pattern GMCVVE = GeoMatchConstraintValue' "VE"

pattern GMCVVN :: GeoMatchConstraintValue
pattern GMCVVN = GeoMatchConstraintValue' "VN"

pattern GMCVVG :: GeoMatchConstraintValue
pattern GMCVVG = GeoMatchConstraintValue' "VG"

pattern GMCVVI :: GeoMatchConstraintValue
pattern GMCVVI = GeoMatchConstraintValue' "VI"

pattern GMCVWF :: GeoMatchConstraintValue
pattern GMCVWF = GeoMatchConstraintValue' "WF"

pattern GMCVEH :: GeoMatchConstraintValue
pattern GMCVEH = GeoMatchConstraintValue' "EH"

pattern GMCVYE :: GeoMatchConstraintValue
pattern GMCVYE = GeoMatchConstraintValue' "YE"

pattern GMCVZM :: GeoMatchConstraintValue
pattern GMCVZM = GeoMatchConstraintValue' "ZM"

pattern GMCVZW :: GeoMatchConstraintValue
pattern GMCVZW = GeoMatchConstraintValue' "ZW"

{-# COMPLETE
  GMCVAF,
  GMCVAX,
  GMCVAL,
  GMCVDZ,
  GMCVAS,
  GMCVAD,
  GMCVAO,
  GMCVAI,
  GMCVAQ,
  GMCVAG,
  GMCVAR,
  GMCVAM,
  GMCVAW,
  GMCVAU,
  GMCVAT,
  GMCVAZ,
  GMCVBS,
  GMCVBH,
  GMCVBD,
  GMCVBB,
  GMCVBY,
  GMCVBE,
  GMCVBZ,
  GMCVBJ,
  GMCVBM,
  GMCVBT,
  GMCVBO,
  GMCVBQ,
  GMCVBA,
  GMCVBW,
  GMCVBV,
  GMCVBR,
  GMCVIO,
  GMCVBN,
  GMCVBG,
  GMCVBF,
  GMCVBI,
  GMCVKH,
  GMCVCM,
  GMCVCA,
  GMCVCV,
  GMCVKY,
  GMCVCF,
  GMCVTD,
  GMCVCL,
  GMCVCN,
  GMCVCX,
  GMCVCC,
  GMCVCO,
  GMCVKM,
  GMCVCG,
  GMCVCD,
  GMCVCK,
  GMCVCR,
  GMCVCI,
  GMCVHR,
  GMCVCU,
  GMCVCW,
  GMCVCY,
  GMCVCZ,
  GMCVDK,
  GMCVDJ,
  GMCVDM,
  GMCVDO,
  GMCVEC,
  GMCVEG,
  GMCVSV,
  GMCVGQ,
  GMCVER,
  GMCVEE,
  GMCVET,
  GMCVFK,
  GMCVFO,
  GMCVFJ,
  GMCVFI,
  GMCVFR,
  GMCVGF,
  GMCVPF,
  GMCVTF,
  GMCVGA,
  GMCVGM,
  GMCVGE,
  GMCVDE,
  GMCVGH,
  GMCVGI,
  GMCVGR,
  GMCVGL,
  GMCVGD,
  GMCVGP,
  GMCVGU,
  GMCVGT,
  GMCVGG,
  GMCVGN,
  GMCVGW,
  GMCVGY,
  GMCVHT,
  GMCVHM,
  GMCVVA,
  GMCVHN,
  GMCVHK,
  GMCVHU,
  GMCVIS,
  GMCVIN,
  GMCVId,
  GMCVIR,
  GMCVIQ,
  GMCVIE,
  GMCVIM,
  GMCVIL,
  GMCVIT,
  GMCVJM,
  GMCVJP,
  GMCVJE,
  GMCVJO,
  GMCVKZ,
  GMCVKE,
  GMCVKI,
  GMCVKP,
  GMCVKR,
  GMCVKW,
  GMCVKG,
  GMCVLA,
  GMCVLV,
  GMCVLB,
  GMCVLS,
  GMCVLR,
  GMCVLY,
  GMCVLI,
  GMCVLT,
  GMCVLU,
  GMCVMO,
  GMCVMK,
  GMCVMG,
  GMCVMW,
  GMCVMY,
  GMCVMV,
  GMCVML,
  GMCVMT,
  GMCVMH,
  GMCVMQ,
  GMCVMR,
  GMCVMU,
  GMCVYT,
  GMCVMX,
  GMCVFM,
  GMCVMD,
  GMCVMC,
  GMCVMN,
  GMCVME,
  GMCVMS,
  GMCVMA,
  GMCVMZ,
  GMCVMM,
  GMCVNA,
  GMCVNR,
  GMCVNP,
  GMCVNL,
  GMCVNC,
  GMCVNZ,
  GMCVNI,
  GMCVNE,
  GMCVNG,
  GMCVNU,
  GMCVNF,
  GMCVMP,
  GMCVNO,
  GMCVOM,
  GMCVPK,
  GMCVPW,
  GMCVPS,
  GMCVPA,
  GMCVPG,
  GMCVPY,
  GMCVPE,
  GMCVPH,
  GMCVPN,
  GMCVPL,
  GMCVPT,
  GMCVPR,
  GMCVQA,
  GMCVRE,
  GMCVRO,
  GMCVRU,
  GMCVRW,
  GMCVBL,
  GMCVSH,
  GMCVKN,
  GMCVLC,
  GMCVMF,
  GMCVPM,
  GMCVVC,
  GMCVWS,
  GMCVSM,
  GMCVST,
  GMCVSA,
  GMCVSN,
  GMCVRS,
  GMCVSC,
  GMCVSL,
  GMCVSG,
  GMCVSX,
  GMCVSK,
  GMCVSI,
  GMCVSB,
  GMCVSO,
  GMCVZA,
  GMCVGS,
  GMCVSS,
  GMCVES,
  GMCVLK,
  GMCVSD,
  GMCVSR,
  GMCVSJ,
  GMCVSZ,
  GMCVSE,
  GMCVCH,
  GMCVSY,
  GMCVTW,
  GMCVTJ,
  GMCVTZ,
  GMCVTH,
  GMCVTL,
  GMCVTG,
  GMCVTK,
  GMCVTO,
  GMCVTT,
  GMCVTN,
  GMCVTR,
  GMCVTM,
  GMCVTC,
  GMCVTV,
  GMCVUG,
  GMCVUA,
  GMCVAE,
  GMCVGB,
  GMCVUS,
  GMCVUM,
  GMCVUY,
  GMCVUZ,
  GMCVVU,
  GMCVVE,
  GMCVVN,
  GMCVVG,
  GMCVVI,
  GMCVWF,
  GMCVEH,
  GMCVYE,
  GMCVZM,
  GMCVZW,
  GeoMatchConstraintValue'
  #-}
